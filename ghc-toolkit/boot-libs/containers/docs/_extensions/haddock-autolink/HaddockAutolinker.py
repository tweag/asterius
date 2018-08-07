from docutils import nodes
import itertools
import string

##################################################
### Utility functions.
##################################################

def print_error(err_string):
    """
    Print the err_string in red font.
    """

    FAIL = '\033[91m'
    ENDC = '\033[0m'
    print FAIL + err_string + ENDC


def get_project(inliner):
    """
    Returns the project name associated with the file being processed.
    This is used when generating _implicit_ links.
    """

    return inliner.document.settings.env.app.config.project


def convert_special_chars_to_ascii(func_name):
    """
    Convert non-alphanumeric characters to their ascii representation.

    This is how Haddock generates links for operators.

    '!?' => '-33--63-'
    """

    if func_name == None:
        return None

    escaped_func_name = [ c if c not in string.punctuation else '-' + str(ord(c)) + '-'
                          for c in func_name ]

    return ''.join(escaped_func_name)


def parse_haddock_ref_text(text):
    """
    Parses text of the form pkg-name/Module.Path#ident into the tuple
    (package, module, ident).

    The module and function name are optional, if they are omitted then 'None'
    will be returned in the corresponding tuple element. If this is an implicit
    reference (/Module.Path#ident) then 'package' will be 'None'.

    Example inputs:
      Explicit package references:
      'pkg' => ('package_name', None, None)
      'pkg/Module.Name' => ('pkg', 'Module.Name', None)
      'pkg/Module.Name#ident' => ('pkg', 'Module.Name', 'ident')

      Implicit package references:
      '/Module.Name' => (None, 'Module.Name', None)
      '/Module.Name#ident' => (None, 'Module.Name', 'ident')
    """

    # If there's no '/' then this is a reference to a package.
    # A module or identifier reference is invalid here.
    if '/' not in text:
        if '#' in text:
            print_error('Invalid haddock reference: ' + text)
            raise Exception('Invalid haddock reference, see error above.')
            return (None, None, None)
        else:
            return (text, None, None)

    # Leading '/' means local package reference.
    # Calling code responsible for determining what "local" means.
    if 0 == text.find('/'):
        text = text[1:]
        if '#' in text:
            module,ident = text.split('#')
            return (None, module, ident)
        else:
            return (None, text, None)

    # Otherwise, explicit reference.
    pkg_name,rest = text.split('/')

    ident = None
    module = None
    if '#' in rest:
        module,ident = rest.split('#')
    else:
        module = rest

    return (pkg_name, module, ident)


##################################################
### Link generation functions.
##################################################


def pkg_root_ref(haddock_host, haddock_root, pkg):
    """
    Returns the URI for the root of pkg's Haddocks.

    Note: Hackage uses a different URI scheme than stackage and local.

    URI enclosed in {} corresponds to 'haddock_root'.

    Hackage: {hackage.haskell.org/package/}<pkg_name>
    Stackage: {www.stackage.org/haddock/<resolver>/}<pkg_name>/index.html
    Local: {file:///path/to/html/}<pkg_name>/index.html
    """

    if haddock_host == 'hackage':
        return haddock_root + pkg

    if haddock_host == 'stackage':
        return haddock_root + pkg + '/index.html'

    if haddock_host == 'local':
        return haddock_root + pkg + '/index.html'


def module_ref(haddock_host, haddock_root, pkg, module, func_name):
    """
    Returns the URI referring to pkg/module#func_name.

    Note: Hackage uses a different URI scheme than stackage and local.

    URI enclosed in {} corresponds to 'haddock_root'.

    Hackage: {hackage.haskell.org/package/}<pkg_name>/docs/<module>.html#v:<func_name>
    Stackage: {www.stackage.org/haddock/<resolver>/}<pkg_name>/<module>.html#t:<func_name>
    Local: {file:///path/to/html/}<pkg_name>/<module>.html#t:<func_name>
    """

    if module != None:
        module = module.replace('.', '-')

    if haddock_host == 'hackage':
        ref = haddock_root + pkg + '/docs/' + module + '.html'

    if haddock_host == 'stackage':
        ref = haddock_root + pkg + '/' + module + '.html'

    if haddock_host == 'local':
        ref = haddock_root + pkg + '/' + module + '.html'

    # If a function name was provided, link to it.
    if func_name != None:
        # Select the correct anchor, types use #t, functions use #v.
        # TODO(m-renuad): Determine if there's cases where this is incorrect.
        if func_name[0].isupper():
            anchor_type = '#t:'
        else:
            anchor_type = '#v:'
        ref = ref + anchor_type + func_name

    return ref


def haddock_ref(haddock_host, haddock_root, pkg, module, func_name):
    """
    Return a reference link to Haddocks for pkg/module#func_name.
    """

    if module == None and func_name == None:
        return pkg_root_ref(haddock_host, haddock_root, pkg)
    else:
        func_name = convert_special_chars_to_ascii(func_name)
        return module_ref(haddock_host, haddock_root, pkg, module, func_name)



#############################################
# -- Custom roles for linking to Hackage docs
#############################################

# Support building docs with link to hackage, stackage, or locally build haddocks.
# Valid options:
#   hackage - link to hackage.haskell.org
#   stackage - link to www.stackage.org (must also pass STACKAGE_RESOLVER)
#   local - any path to local docs (must also set HADDOCK_DIR)
#
# Note: Defaults to hackage if none specified.
#
# Note: We need to do some custom URL rewriting for stackage because it uses a different
# format from what the haddock tool builds
#
# TODO(m-renaud): Improve this and publish as sphinx extension.

### URI scheme examples for Hackage, Stackage, and local docs.
## Packages
# Hackage: hackage.haskell.org/package/containers
# Stackage: www.stackage.org/haddock/lts-10.0/containers/index.html
# Local: file:///local/path/html/containers/index.html

## Module (and function) references
# Hackage: hackage.haskell.org/package/containers/docs/Data.Set.html#v:empty
# Stackage: www.stackage.org/haddock/lts-10.0/containers/Data.Set.html#t:empty
# Local: file:///path/to/html/containers/Data.Set.html#t:empty

class HaddockAutolinker:

    def __init__(self, haddock_host, haddock_root):
        self.haddock_host = haddock_host
        self.haddock_root = haddock_root


    def haddock_role(self, display_name_only=False):
        def haddock_role_impl(name, rawtext, text, lineno, inliner, options={}, content=[]):
            """
            Role handler for :haddock:.
            """

            (pkg, module, ident) = parse_haddock_ref_text(text)

            # If the pkg isn't set then this is a local reference to a module
            # function in the current package.
            if pkg == None:
                pkg = get_project(inliner)
            ref = haddock_ref(self.haddock_host, self.haddock_root, pkg, module, ident)

            if ref == None:
                print_error('ERROR: invalid argument to :' + name + ':')
                print_error('  Markup: ' + str(rawtext))
                print_error('  Line: ' + str(lineno))
                raise Exception('Invalid Haddock link, see ERROR above.')

            if module == None:
                link_text = pkg
            else:
                if ident == None:
                    link_text = module
                else:
                    if display_name_only:
                        link_text = ident
                    else:
                        link_text = module + '#' + ident

            node = nodes.reference(rawtext, link_text, refuri=ref, **options)
            return [node], []
        return haddock_role_impl
