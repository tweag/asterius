import Asterius.Types
import Diagrams.Backend.SVG
import Diagrams.Prelude

hilbert :: Int -> Trail V2 Double
hilbert 0 = mempty
hilbert n =
  hilbert' (n - 1)
    # reflectY
    <> vrule 1
    <> hilbert (n - 1)
    <> hrule 1
    <> hilbert (n - 1)
    <> vrule (-1)
    <> hilbert' (n - 1)
    # reflectX
  where
    hilbert' m = hilbert m # rotateBy (1 / 4)

example :: Diagram B
example = frame 1 . lw medium . lc darkred . strokeT $ hilbert 5

foreign import javascript
   "(() => {                                    \
   \   const d = document.createElement('div'); \
   \   d.innerHTML = $1;                      \
   \   document.body.appendChild(d);            \
   \ })()"
   showSVG :: JSString -> IO ()

main :: IO ()
main = do
  let opts = SVGOptions
        { _size = dims2D 400 400,
          _svgDefinitions = Nothing,
          _idPrefix = mempty,
          _svgAttributes = [],
          _generateDoctype = False
        }
      svg = renderDia SVG opts example
  showSVG (toJSString (show svg))
