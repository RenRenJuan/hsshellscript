module Main (main) where

import HsShellScript --hiding ({-make_usage_info, print_usage_info, -}argname_short)
import Data.Maybe
import Data.List
import Data.Char
import Debug.Trace
import Control.Monad
import Control.Exception
import System.IO


header                                  = "Testprogramm für Kommandozeilenargumente\n\n"
descs                                   = [ d_komp, d_trenner, d_pfade, d_uml, d_inhalt, d_bla ]
args                                    = unsafe_getargs header descs
trenner_normal                          = " - "

pfade                                   = args_req args d_pfade
trenner                                 = fromMaybe trenner_normal $ optarg_req args d_trenner
inhalt                                  = arg_switch args d_inhalt
komp                                    = optarg_req args d_komp


d_weg  =
   argdesc [ desc_short 'w'
           , desc_long "weg"
           , desc_at_most_once
           , desc_argname "Teiltext"
           , desc_value_required
           , desc_description "xxx" --Teiltext aus den Dateinamen entfernen")
           ]


d_komp =
   argdesc [ desc_short 'k'
           , desc_short 'l'
           , desc_short 'm'
           , desc_long "komp"
           , desc_long "komp1"
           , desc_long "komp2"
           , desc_value_required
           , desc_argname "<Name>"
           , desc_description "Das ist eine lange Argumentbeschreibung, die umgebrochen werden muß, weil sie zu lang ist. Wirklich. Undhierkommteinganzlangeswortdasgetrenntwerdenmuß"
           ]

d_trenner =
   argdesc [ desc_short 't'
           , desc_long "langer-param"
           , desc_at_most_once
           , desc_value_required
           , desc_argname "<Txt>"
           , desc_description "Diese Beschreibung\nhat mehrere\nZeilen."
           ]

d_uml =
   argdesc [ desc_long "äöüß"
           , desc_at_most_once
           ]

d_pfade =
   argdesc [ desc_direct
           , desc_any_times
           , desc_description ("Pfade, denen vorgesetzt werden soll; bzw, bla bla Verzeichnisse, die ihren Inhalten vorgesetzt werden sollen")
           ]

d_inhalt =
   argdesc [ desc_long "inhalt"
           , desc_description ("Nicht angegebenen Dateien vorsetzen, sondern dem Inhalt der angegebenen Verzeichnisse. Angegebene Nicht-Verzeichnisse \
                              \beiseitelassen.")
           ]

d_bla = 
   argdesc [ desc_long "foo"
           , desc_short 'h'
           , desc_value_required
           , desc_argname "Name"
           , desc_description "bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla \
                              \bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla "
           ]


main = mainwrapper $ do
   seq args (return ())

   --anz d_weg
   --print (argname_long d_weg)

   --let zll = make_usage_info [d_weg] 0 15 30 80
   --mapM_ print zll

   print_usage_info stdout "Header\n\n" [d_weg] --descs



anz :: ArgumentDescription 
    -> IO ()
anz desc = do 
   outm ("argdesc_short_args = " ++ show (argdesc_short_args desc))
   outm ("argdesc_long_args = " ++ show (argdesc_long_args desc))
   outm ("argdesc_argarg = " ++ show (argdesc_argarg desc))
   outm ("argdesc_times = " ++ show (argdesc_times desc))
   outm ("argdesc_argargname = " ++ show (argdesc_argargname desc))
   outm ("argdesc_description = " ++ show (argdesc_description desc))
