# -*- coding: utf-8 -*-

"""
The MIT License (MIT)

Copyright (c) 2015-2017 Danny Y.

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the "Software"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Python module that generates .ninja files.

Some of the stuff here was borrowed from the main ninja project.
However this module aims to extend the bare-bones provided with a rich
python API.

Note: This file is meant to be a single file for easy inclusion for existing
build systems. Hence this file will be rather big.

"""

import os
import io
import re
import sys
import glob
import shlex
import errno
import fnmatch
import textwrap
import datetime

##############################################################################
# The following functions are backports for Python 2.x. Some are modified
# from the original CPython repository, however retrofitted and backported to
# Python 2.x themselves.
##############################################################################

_find_unsafe = re.compile(r'[^a-zA-Z0-9_@%+=:,./-]').search

def quote(s):
    """Return a shell-escaped version of the string *s*."""
    if not s:
        return "''"
    if _find_unsafe(s) is None:
        return s

    # use single quotes, and put single quotes into double quotes
    # the string $'b is then quoted as '$'"'"'b'
    return "'" + s.replace("'", "'\"'\"'") + "'"

try:
    _string_types = basestring
except NameError:
    _string_types = str

try:
    from cStringIO import StringIO as _StringIO
except ImportError:
    _StringIO = io.StringIO

# Check that a given file can be accessed with the correct mode.
# Additionally check that `file` is not a directory, as on Windows
# directories pass the os.access check.
def _access_check(fn, mode):
    return os.path.exists(fn) and os.access(fn, mode) and not os.path.isdir(fn)

def _ensure_path(path):
    try:
        os.makedirs(path)
    except OSError as exception:
        if exception.errno != errno.EEXIST:
            raise

def executables_in_path(matcher=None):
    path = os.environ.get("PATH", os.defpath)

    if path is None:
        return

    if matcher is None:
        matcher = lambda c: True

    path = path.split(os.pathsep)

    if sys.platform == 'win32':
        if not os.curdir in path:
            path.insert(0, os.curdir)

    mode = os.F_OK | os.X_OK
    for directory in path:
        if not os.path.exists(directory):
            continue

        for exe in os.listdir(directory):
            if not matcher(exe):
                continue

            fn = os.path.realpath(os.path.join(directory, exe))
            if _access_check(fn, mode):
                yield fn

def which(cmd):
    """Given a command, mode, and a PATH string, return the path which
    conforms to the given mode on the PATH, or None if there is no such
    file.

    `mode` defaults to os.F_OK | os.X_OK. `path` defaults to the result
    of os.environ.get("PATH"), or can be overridden with a custom search
    path.

    """

    # If we're given a path with a directory part, look it up directly rather
    # than referring to PATH directories. This includes checking relative to the
    # current directory, e.g. ./script
    if os.path.dirname(cmd):
        if _access_check(cmd, mode):
            return cmd
        return None

    path = os.environ.get("PATH", os.defpath)
    mode = os.F_OK | os.X_OK
    if not path:
        return None
    path = path.split(os.pathsep)

    if sys.platform == "win32":
        # The current directory takes precedence on Windows.
        if not os.curdir in path:
            path.insert(0, os.curdir)

        # PATHEXT is necessary to check on Windows.
        pathext = os.environ.get("PATHEXT", "").split(os.pathsep)
        # See if the given file matches any of the expected path extensions.
        # This will allow us to short circuit when given "python.exe".
        # If it does match, only test that one, otherwise we have to try
        # others.
        if any(cmd.lower().endswith(ext.lower()) for ext in pathext):
            files = [cmd]
        else:
            files = [cmd + ext for ext in pathext]
    else:
        # On other platforms you don't have things like PATHEXT to tell you
        # what file suffixes are executable, so just pass on cmd as-is.
        files = [cmd]

    seen = set()
    for dir in path:
        normdir = os.path.normcase(dir)
        if not normdir in seen:
            seen.add(normdir)
            for thefile in files:
                name = os.path.join(dir, thefile)
                if _access_check(name, mode):
                    return name
    return None



def escape_path(word):
    return word.replace('$ ', '$$ ').replace(' ', '$ ').replace(':', '$:')

class Writer(object):
    def __init__(self, filename, mode='w', **kwargs):
        if isinstance(filename, _string_types):
            self.fp = io.open(filename, mode)
        else:
            self.fp = filename # probably a file-like object

        self.width = kwargs.get('width', 78)

    def __enter__(self):
        return self

    def __exit__(self, type, exc, tb):
        self.close()

    def newline(self):
        self.fp.write('\n')

    def comment(self, text, has_path=False):
        for line in textwrap.wrap(text, self.width - 2, break_long_words=False,
                                  break_on_hyphens=False):
            self.fp.write('# ' + line + '\n')

    def variable(self, key, value, indent=0):
        if value is None:
            return
        if isinstance(value, list):
            value = ' '.join(filter(None, value))  # Filter out empty strings.
        self._line('%s = %s' % (key, value), indent)

    def pool(self, name, depth):
        self._line('pool %s' % name)
        self.variable('depth', depth, indent=1)

    def rule(self, name, command, description=None, depfile=None,
             generator=False, pool=None, restat=False, rspfile=None,
             rspfile_content=None, deps=None):
        self._line('rule %s' % name)
        self.variable('command', command, indent=1)
        if description:
            self.variable('description', description, indent=1)
        if depfile:
            self.variable('depfile', depfile, indent=1)
        if generator:
            self.variable('generator', '1', indent=1)
        if pool:
            self.variable('pool', pool, indent=1)
        if restat:
            self.variable('restat', '1', indent=1)
        if rspfile:
            self.variable('rspfile', rspfile, indent=1)
        if rspfile_content:
            self.variable('rspfile_content', rspfile_content, indent=1)
        if deps:
            self.variable('deps', deps, indent=1)

    def build(self, outputs, rule, inputs=None, implicit=None, order_only=None,
              variables=None, implicit_outputs=None):
        outputs = self._as_list(outputs)
        out_outputs = [escape_path(x) for x in outputs]
        all_inputs = [escape_path(x) for x in self._as_list(inputs)]

        if implicit:
            implicit = [escape_path(x) for x in self._as_list(implicit)]
            all_inputs.append('|')
            all_inputs.extend(implicit)
        if order_only:
            order_only = [escape_path(x) for x in self._as_list(order_only)]
            all_inputs.append('||')
            all_inputs.extend(order_only)
        if implicit_outputs:
            implicit_outputs = [escape_path(x)
                                for x in self._as_list(implicit_outputs)]
            out_outputs.append('|')
            out_outputs.extend(implicit_outputs)

        self._line('build %s: %s' % (' '.join(out_outputs),
                                     ' '.join([rule] + all_inputs)))

        if variables:
            if isinstance(variables, dict):
                iterator = iter(variables.items())
            else:
                iterator = iter(variables)

            for key, val in iterator:
                self.variable(key, val, indent=1)

        return outputs

    def include(self, path):
        self._line('include %s' % path)

    def subninja(self, path):
        self._line('subninja %s' % path)

    def default(self, paths):
        self._line('default %s' % ' '.join(self._as_list(paths)))

    def _count_dollars_before_index(self, s, i):
        """Returns the number of '$' characters right in front of s[i]."""
        dollar_count = 0
        dollar_index = i - 1
        while dollar_index > 0 and s[dollar_index] == '$':
            dollar_count += 1
            dollar_index -= 1
        return dollar_count

    def _line(self, text, indent=0):
        """Write 'text' word-wrapped at self.width characters."""
        leading_space = '  ' * indent
        while len(leading_space) + len(text) > self.width:
            # The text is too wide; wrap if possible.

            # Find the rightmost space that would obey our width constraint and
            # that's not an escaped space.
            available_space = self.width - len(leading_space) - len(' $')
            space = available_space
            while True:
                space = text.rfind(' ', 0, space)
                if (space < 0 or
                    self._count_dollars_before_index(text, space) % 2 == 0):
                    break

            if space < 0:
                # No such space; just use the first unescaped space we can find.
                space = available_space - 1
                while True:
                    space = text.find(' ', space + 1)
                    if (space < 0 or
                        self._count_dollars_before_index(text, space) % 2 == 0):
                        break
            if space < 0:
                # Give up on breaking.
                break

            self.fp.write(leading_space + text[0:space] + ' $\n')
            text = text[space+1:]

            # Subsequent lines are continuations, so indent them.
            leading_space = '  ' * (indent+2)

        self.fp.write(leading_space + text + '\n')

    def close(self):
        self.fp.close()

    def escape(self, string):
        """Escape a string such that it can be embedded into a Ninja file without
        further interpretation."""
        assert '\n' not in string, 'Ninja syntax does not allow newlines'
        # We only have one special metacharacter: '$'.
        return string.replace('$', '$$')

    def expand(self, string, vars, local_vars={}):
        """Expand a string containing $vars as Ninja would.

        Note: doesn't handle the full Ninja variable syntax, but it's enough
        to make configure.py's use of it work.
        """
        def exp(m):
            var = m.group(1)
            if var == '$':
                return '$'
            return local_vars.get(var, vars.get(var, ''))
        return re.sub(r'\$(\$|\w*)', exp, string)

    def _as_list(self, l):
        if l is None:
            return []
        if isinstance(l, list):
            return l
        return [l]

class Compiler(object):
    """Represents a C++ compiler object.

    By default, this is a GCC-like compiler.
    """

    def __init__(self, name):
        self.name = name

    def __eq__(self, other):
        return isinstance(other, Compiler) and other.name == self.name

    def __ne__(self, other):
        return not isinstance(other, Compiler) or other.name != self.name

    def __hash__(self):
        return hash(self.name)

    def __repr__(self):
        return '<Compiler name=%r>' % self.name

    def setup_rules(self, ninja):
        ninja.rule('compile', command='$cxx -MMD -MF $out.d -c $cxxflags $includes $depends $defines $in -o $out',
                              deps='gcc', depfile='$out.d',
                              description='Compiling $in to $out')
        ninja.rule('link', command='$cxx $cxxflags $in -o $out $linkflags $libpath $libraries',
                           description='Creating $out')

    @classmethod
    def detected(cls):
        """Returns Compiler instances that are detected on the machine."""
        matcher = lambda g: g.startswith(('g++', 'clang++'))
        return { cls(exe) for exe in executables_in_path(matcher) }

    @classmethod
    def from_name(cls, name):
        exe = which(name)
        if exe is None:
            raise RuntimeError('Compiler %s not found in PATH' % name)
        return cls(exe)

    def object_file(self, filename):
        (root, _) = os.path.splitext(filename)
        return root + '.o'

    def include(self, path):
        return '-I' + quote(path)

    def libpath(self, path):
        return '-L' + quote(path)

    def library(self, lib):
        return '-l' + lib

    def dependency(self, dep):
        return '-isystem ' + quote(dep)

    def define(self, macro):
        return '-D ' + macro

gcc = Compiler('g++')
clang = Compiler('clang++')

class CompilationSettings(object):
    def __init__(self):
        self._flags = []
        self._link_flags = []
        self._libraries = []
        self._dependencies = []
        self._includes = []
        self._defines = []

    def add_libraries(self, *libraries):
        if libraries:
            self._libraries.extend(libraries)

    def add_flags(self, *flags):
        if flags:
            self._flags.extend(flags)

    def add_includes(self, *includes):
        if includes:
            self._includes.extend(includes)

    def add_defines(self, *defines):
        pass

class Executable(CompilationSettings):
    """Represents an executable being generated."""

    def __init__(self, name, **kwargs):
        super(Executable, self).__init__()
        self.name = name
        self.output_directory = os.path.abspath(kwargs.pop('output_directory', '.'))
        self.phony = kwargs.pop('phony', None)
        self._files = []
        self._runners = []

    def add_files(self, files):
        if isinstance(files, _string_types):
            files = glob.glob(files)

        if files:
            self._files.extend(files)

    def add_file(self, file):
        self._files.append(file)

    def create_runner(self, name, arguments=None):
        if arguments is None:
            t = (name, [])
        else:
            t = (name, list(arguments))

        self._runners.append(t)

    def _generate(self, project):
        if not self._files:
            return

        compiler = project.compiler

        variables = {
            'cxxflags': ' '.join(self._flags),
            'includes': ' '.join(compiler.include(a) for a in self._includes),
            'depends': ' '.join(compiler.dependency(a) for a in self._dependencies),
            'libraries': ' '.join(compiler.library(a) for a in self._libraries),
            'defines': ' '.join(compiler.define(a) for a in self._defines),
        }

        object_files = []
        object_directory = os.path.join(self.output_directory, 'obj')

        _ensure_path(object_directory)

        # add compile rules for every file in the executable
        for f in self._files:
            obj = os.path.normpath(os.path.join(object_directory, compiler.object_file(f)))
            project.build(obj, 'compile', inputs=f, variables=variables)
            object_files.append(obj)

        # link the files together
        resulting_binary = os.path.normpath(os.path.join(self.output_directory, self.name))
        project.build(resulting_binary, 'link', inputs=object_files, variables=variables)

        if self.phony is not None:
            project.build(self.phony, 'phony', inputs=resulting_binary)

        for name, arguments in self._runners:
            runner_name = '%s_%s_runner' % (self.name, name)
            args = [resulting_binary]
            args.extend(quote(a) for a in arguments)
            project.rule(runner_name, command=' '.join(args))
            project.build(name, runner_name, implicit=resulting_binary)

        project.newline()

class Project(Writer):
    """Maintains the project.

    Can contain different executables or libraries.

    This is a subclass of the Ninja writer.
    """

    def __init__(self, filename, **kwargs):
        compiler = kwargs.pop('compiler', None)
        if not isinstance(compiler, Compiler):
            raise TypeError('compiler must be Compiler, not ' + type(compiler).__name__)

        self.compiler = compiler
        self._original_filename = filename
        self._executables = []
        fp = _StringIO()
        super(Project, self).__init__(fp, **kwargs)

    def __del__(self):
        # just in case
        self.close()

    def generate(self, **kwargs):
        """Generates the .ninja file.

        This can only be done once.
        """

        # keyword only argument emulation for Python 2.x
        bootstrap = kwargs.pop('bootstrap', False)
        if len(kwargs) != 0:
            raise TypeError('generate() got unexpected keyword arguments')

        now = datetime.datetime.utcnow()
        self.comment('This file was generated automatically on %s UTC using ' \
                     'the sen.py script. Do not modify.' % now.isoformat())

        self.newline()
        self.variable('ninja_required_version', '1.3')
        self.variable('cxx', self.compiler.name)

        # TODO: builddir variable?

        self.newline()
        self.compiler.setup_rules(self)
        self.newline()

        if bootstrap:
            argv = sys.argv[:]
            argv.insert(0, sys.executable)
            self.rule('bootstrap', command=' '.join(argv), generator=True)
            self.build(self._original_filename, 'bootstrap', implicit=argv[1])

        for executable in self._executables:
            executable._generate(self)

        to_write = self.fp.getvalue().encode('utf-8')

        # if we're here then our generation went okay so replace
        # the file with the actual generated one.
        with io.open(self._original_filename, 'wb') as fp:
            fp.write(to_write)

    def add_executable(self, exe):
        if not isinstance(exe, Executable):
            raise TypeError('expected Executable, received %s instead' % type(exe).__name__)

        self._executables.append(exe)
