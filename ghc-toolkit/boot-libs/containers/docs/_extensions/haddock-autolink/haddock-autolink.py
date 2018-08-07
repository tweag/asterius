from docutils import nodes
from HaddockAutolinker import (HaddockAutolinker, haddock_ref)
import os

def setup(app):
    # Read the HADDOCK_HOST environment variable.
    haddock_host = os.getenv('HADDOCK_HOST', 'hackage')

    # Hackage hosting
    if haddock_host == 'hackage':
        haddock_root = 'https://hackage.haskell.org/package/'

    # Stackage hosting
    elif haddock_host == 'stackage':
        stackage_resolver = os.getenv('STACKAGE_RESOLVER', None)
        if stackage_resolver != None:
            haddock_root = 'https://www.stackage.org/haddock/' + stackage_resolver + '/'
        else:
            raise Exception("Must specify STACKAGE_RESOLVER when setting HADDOCK_HOST=stackage")

    # Local hosting
    elif haddock_host == 'local':
        haddock_dir = os.getenv('HADDOCK_DIR', None)
        if haddock_dir != None:
            haddock_root = haddock_dir
        else:
            raise Exception("Must specify HADDOCK_DIR when setting HADDOCK_HOST=local")

    else:
        raise Exception("HADDOCK_HOST not recognized, valid options: hackage, stackage, local")

    haddock_autolinker = HaddockAutolinker(haddock_host, haddock_root)

    app.add_role('haddock', haddock_autolinker.haddock_role())
    app.add_role('haddock_short', haddock_autolinker.haddock_role(True))

    # Print info about Haddock link generation.
    print '\nHaddock host information'
    print '  haddoc_host: ' + haddock_host
    print '  haddock_root: ' + haddock_root
    print
    print '  Links to docs will be of the form: ' + \
        haddock_ref(haddock_host, haddock_root, 'pkg-name', 'Module-Name', 'funcName')

