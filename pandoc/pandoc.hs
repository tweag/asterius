{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -O2 #-}

import Asterius.Text
import Asterius.Types
import Data.Foldable
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Text.Pandoc

main :: IO ()
main = do
  for_ (M.keys textReaders) $ \(textToJSString -> k) -> sel_add "select_src" k
  for_ (M.keys textWriters) $ \(textToJSString -> k) -> sel_add "select_dst" k
  set_convert =<< mkCallback convert

convert :: JSString -> JSString -> JSString -> IO JSString
convert (textFromJSString -> src_markup) (textFromJSString -> dst_markup) (textFromJSString -> src_txt) =
  fmap textToJSString
    $ runIOorExplode
    $ (textWriters M.! dst_markup) def
      =<< (textReaders M.! src_markup) def src_txt

textReaders :: M.Map T.Text (ReaderOptions -> T.Text -> PandocIO Pandoc)
textReaders =
  M.fromList
    [ ("docbook", readDocBook),
      ("html", readHtml),
      ("latex", readLaTeX),
      ("markdown", readMarkdown),
      ("mediawiki", readMediaWiki),
      ("opml", readOPML),
      ("org", readOrg),
      ("rst", readRST),
      ("t2t", readTxt2Tags),
      ("textile", readTextile),
      ("twiki", readTWiki)
    ]

textWriters :: M.Map T.Text (WriterOptions -> Pandoc -> PandocIO T.Text)
textWriters =
  M.fromList
    [ ("asciidoc", writeAsciiDoc),
      ("docbook", writeDocbook5),
      ("dokuwiki", writeDokuWiki),
      ("html", writeHtml5String),
      ("icml", writeICML),
      ("latex", writeLaTeX),
      ("markdown", writeMarkdown),
      ("mediawiki", writeMediaWiki),
      ("opendocument", writeOpenDocument),
      ("opml", writeOPML),
      ("org", writeOrg),
      ("plain", writePlain),
      ("rst", writeRST),
      ("texinfo", writeTexinfo),
      ("textile", writeTextile)
    ]

foreign import javascript "wrapper"
  mkCallback ::
    (JSString -> JSString -> JSString -> IO JSString) -> IO JSFunction

foreign import javascript unsafe "(() => {      \
\ const opt = document.createElement('option'); \
\ opt.value = $2;                               \
\ opt.appendChild(document.createTextNode($2)); \
\ const sel_src = document.getElementById($1);  \
\ sel_src.appendChild(opt);                     \
\ })()"
  sel_add :: JSString -> JSString -> IO ()

foreign import javascript unsafe "(() => {                                                                                            \
\  const txt = document.getElementById('textarea');                                                                                   \
\  const sel_src = document.getElementById('select_src');                                                                             \
\  const sel_dst = document.getElementById('select_dst');                                                                             \
\  const btn = document.getElementById('button');                                                                                     \
\  btn.addEventListener('click', async () => { txt.value = await ($1(sel_src.value, sel_dst.value, txt.value)).catch(err => err); }); \
\  btn.disabled = false;                                                                                                              \
\  btn.textContent = 'Convert';                                                                                                       \
\ })()"
  set_convert ::
    JSFunction -> IO ()

