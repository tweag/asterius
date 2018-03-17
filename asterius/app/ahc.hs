import Asterius.BuildInfo
import Distribution.Simple.Compiler
import UnliftIO.Environment
import UnliftIO.Process

main :: IO ()
main = do
  ss <- getArgs
  callProcess ghc $
    foldr
      (\arg nargs' ->
         let fp =
               [ "--frontend"
               , "Asterius.FrontendPlugin"
               , "-plugin-package"
               , "asterius"
               ] ++
               case registrationPackageDB packageDBStack of
                 GlobalPackageDB -> ["-global-package-db"]
                 UserPackageDB -> ["-user-package-db"]
                 SpecificPackageDB p -> ["-package-db", p]
          in case arg of
               "--make" -> fp ++ nargs'
               "-c" -> fp ++ nargs'
               _ -> arg : nargs')
      []
      ss
