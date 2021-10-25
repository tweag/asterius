import importlib  
make_packages = importlib.import_module("make-packages")

if __name__ == "__main__":
    # make_packages.ghc_clean()
    #make_packages.make_hadrian()
    make_packages.ghc_configure()
    make_packages.make_autogen()
    make_packages.make_ghc_heap_asterius()
    make_packages.make_ghc_boot_th_asterius()
    make_packages.make_ghc_boot_asterius()
    make_packages.make_template_haskell_asterius()
    make_packages.make_ghci_asterius()
    make_packages.make_ghc_asterius()
