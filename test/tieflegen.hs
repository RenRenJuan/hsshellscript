-- tieflegen: Dateinamenteile zu Oberverzeichnissen machen

import System.Directory
import Control.Monad
import Data.Maybe
import System.IO
import System.IO.Error hiding (catch)
import System.Posix
import Data.List
import Text.Regex
import Debug.Trace
import Prelude hiding (catch)

--import Text hiding (zerlegen)
--import Kommandos
--import Arg hiding (d_probe)
--import Hsskripte
--import Datenbank
import HsShellScript hiding (echo)



header           = "tieflegen [Optionen] [Verzeichnisse]"
args             = unsafe_getargs header descs
descs            = [ d_trenner, d_verzl, d_ausrollen, d_verschieben, d_tiefe, d_gk_beachten, d_probe, d_umbenennen, d_ebenen,
                     d_spalten, d_album
                   ] ++ map za_arg zerlegungsarten

trenner_normal   = " - "
trennzeichen     = "-_ ~()"

trenner          = fromMaybe trenner_normal $ optarg_req args d_trenner
tiefe            = if arg_switch args d_album then 2 else read (fromMaybe "1" $ optarg_req args d_tiefe)
pfade            = args_req args d_verzl
probe            = arg_switch args d_probe
beachten         = arg_switch args d_gk_beachten
umbenennen       = arg_switch args d_umbenennen
ebenen           = if arg_switch args d_album then 1 else read (fromMaybe "0" (optarg_req args d_ebenen))
spalten          = arg_switch args d_spalten || arg_switch args d_album


------------------------------------------------------------------------------------------------------------------------------------------------------
-- Möglichkeiten zu zerlegen. Hieraus Kommandozeilenparameter u.a.

-- Wenn nichts anderes gewählt
normalzerleger    = trenner_zerleger trenner
normalverschieben = False


zerlegungsarten :: [ ( ArgumentDescription                                                      -- Kommandozeilenparameter
                     , ArgumentDescription -> Arguments -> (Bool -> String -> [String])         -- daraus Zerleger
                     , Bool                                                                     -- ob normal verschieben
                     )
                   ]
zerlegungsarten =
   [
   -- Nach regulärem Ausdruck
     ( argdesc [ desc_short 'r'
               , desc_long "regex"
               , desc_argname "Regex"
               , desc_value_required
               , desc_description $ "Teile durch reg. Ausdr. auszeichnen. Abgeglichen wird ein passender TEIL des Namens, nicht der ganze Namen.\n\
                                    \      Bei " ++ argname d_verschieben ++ " sind nur die beabsichtigten Verzeichnisteile auszuzeichnen, sonst \n\
                                    \      Verzeichnisteile und Dateinameteil. Außer bei " ++ argname d_ausrollen ++ " wird VERSCHOBEN."
               ]
     , \arg args -> regex_zerleger_t (fromJust (optarg_req args arg)) beachten
     , True
     )

   -- An letztem Vorkommen des Trenners oder eines Spalters
   , ( argdesc [ desc_short 's'
               , desc_long "spalten-an"
               , desc_argname "Spalter"
               , desc_value_required
               , desc_description $ "Am letzten Vorkommen des Spalters (normal der Trenner) spalten. Trennzeichen "
                                    ++ quote trennzeichen ++ " fallenlassen. Außer\n      bei " ++ argname d_ausrollen ++ " wird VERSCHOBEN."
               ]
     , \arg args -> let tt = fromJust (optarg_req args arg)
                    in  if null tt then throw (Meldung "Fehler: leerer Spalter")
                                   else regex_zerleger_t ("^(.*)[" ++ trennzeichen ++ "]*" ++ intersperse '\\' tt) beachten
     , True
     )

   -- Vor der ersten Zahl spalten
   , ( argdesc [ desc_short 'z'
               , desc_long "vor-zahl"
               , desc_description $ "Den Teil vor der ersten Zahl zum Verzeichnisnamen machen. Außer bei\n      " ++ argname d_ausrollen ++
                                         " wird VERSCHOBEN."
               ]
     , \arg args -> \versch txt -> case regex_zerleger_t "^([^0-9]+)[0-9]" beachten versch txt of
                                      []     -> []
                                      [teil] -> [reverse (dropWhile (`elem` trennzeichen) (reverse teil))]
     , True
     )

   -- Bis zur ersten Zahl zum Verzeichnisnamen machen
   , ( argdesc [ desc_long "bis-zahl"
               , desc_description $ "Den Teil bis zur ersten Zahl zum Verzeichnisnamen machen. Außer bei\n      " ++ argname d_ausrollen ++
                                         " wird VERSCHOBEN."
               ]
     , \arg args -> \versch txt -> case regex_zerleger_t "^([^0-9]+[0-9]*)" beachten versch txt of
                                      []     -> []
                                      [teil] -> [reverse (dropWhile (`elem` trennzeichen) (reverse teil))]
     , True
     )

   -- Vor der letzten Zahl spalten
   , ( argdesc [ desc_short 'l'
               , desc_long "vor-letzter-zahl"
               , desc_description $ "Den Teil vor der letzten Zahl zum Verzeichnisnamen machen. Außer bei\n      " ++ argname d_ausrollen ++
                                         " wird VERSCHOBEN."
               ]
     , \arg args -> \versch txt -> case regex_zerleger_t "^(.*[^0-9])[0-9]" beachten versch txt of
                                      []     -> []
                                      [teil] -> [reverse (dropWhile (`elem` trennzeichen) (reverse teil))]
     , True
     )

   -- Die erste Zahl als Verzeichnisname
   , ( argdesc [ desc_short 'e'
               , desc_long "erste-zahl"
               , desc_description $ "Die erste Zahl zum Verzeichnisnamen machen. Außer bei\n      " ++ argname d_ausrollen ++
                                         "wird VERSCHOBEN."
               ]
     , \arg args -> regex_zerleger_t "([0-9]+)" beachten
     , True
     )
   ]


za_arg         (arg, zerlegerf, verschieben) = arg
za_zerlegerf   (arg, zerlegerf, verschieben) = zerlegerf
za_verschieben (arg, zerlegerf, verschieben) = verschieben



------------------------------------------------------------------------------------------------------------------------------------------------------
-- Kommandozeilenparameter

d_trenner =
   d_trenner_f trenner_normal

d_verzl =
   argdesc [ desc_direct
           , desc_at_least 1
           , desc_description ("Pfade, die getieft werden sollen")
           ]

d_tiefe =
   argdesc [ desc_at_most_once
           , desc_long "tiefe"
           , desc_value_required
           , desc_integer
           , desc_argname "Ebenen"
           , desc_description ("Bis zu so vielen Ebenen tief in Verzeichnisse verwandeln (normal eine)")
           ]

d_verschieben =
   argdesc [ desc_short 'v'
           , desc_long "verschieben"
           , desc_description ("Nur verschieben, nicht umbenennen")
           ]

d_ausrollen =
   argdesc [ desc_short 'a'
           , desc_long "ausrollen"
           , desc_description ("Namensteile zu Verzeichnissen machen und entfernen (normal nur verschieben).")
           ]

d_gk_beachten =
   argdesc [ desc_short 'b'
           , desc_long "beachten"
           , desc_description ("Groß/Kleinschreibung beachten (normal nicht beachten)")
           ]

d_probe =
   argdesc [ desc_short 'p'
           , desc_long "probe"
           , desc_description ("Probelauf - ausgeben wie umbenannt würde")
           ]

d_umbenennen =
   argdesc [ desc_at_most_once
           , desc_short 'U'
           , desc_long "umbenennen"
           , desc_description ("Im Falle einer Kollision umbenennen")
           ]

d_ebenen =
   argdesc [ desc_at_most_once
           , desc_short 'V'
           , desc_long "verschmelzen"
           , desc_argname "Ebenen"
           , desc_description ("Höchstens so viele Ebenen tief verschmelzen, danach stattdessen umbenennen.")
           , desc_nonneg_integer
           , desc_value_required
           ]

d_spalten = 
   argdesc [ desc_at_most_once
           , desc_long "spalten"
           , desc_description ("Ein zwei Teile spalten, d.h. alle abgetrennten Teile vereinen")
           ]

d_album = 
   argdesc [ desc_at_most_once
           , desc_long "album"
           , desc_description ("Kurz für \"--spalten --tiefe=2 --verschmelzen=1\"")
           ]


------------------------------------------------------------------------------------------------------------------------------------------------------
-- Hauptteil

main = mainwrapper $ do
   seq args (return ())
   args_at_most_one (map za_arg zerlegungsarten) args
   args_at_most_one [d_verschieben, d_ausrollen] args
   -- ? args_at_most_one [d_tiefe, d_album, d_ebenen, d_verschieben] args
   hSetBuffering stdout (BlockBuffering (Just 10000))

   outm "-01-"  --XX
   let -- Zerleger aus den Kommandozeilenparametern bestimmen
       (zerleger, zerl_verschieben) =
          case filter (\za -> arg_occurs args (za_arg za)) zerlegungsarten of
             [za] -> ( za_zerlegerf za (za_arg za) args, za_verschieben za )
             []   -> ( normalzerleger, normalverschieben )

       -- Ob verschoben wird aus den Kommandozeilenparametern bestimmen
       verschieben = arg_occurs args d_album ||
                     case (arg_switch args d_ausrollen, arg_switch args d_verschieben) of
                        (True, False)  -> False
                        (False, True)  -> True
                        (False, False) -> zerl_verschieben

   outm "-02-"  --XX
   if probe
      then do umbl <- Main.ausfuehren zerleger
                                      verschieben
                                      (\pfad verzteile neudn -> return (pfad, verzteile ++ [neudn]))
                                      pfade
              let tab = tabelle (repeat (\st feld -> gelb (linksbuendig st feld)))
                                [gelb "alt", gelb "Neu"]
                                (repeat linksbuendig)
                                (map (\(pfad, teile) -> [pfad, concat (intersperse (hell "/") teile)]) umbl)
              putStr (unlines tab)

      else do zs <- zeitstempel
              outm "-03-"  --XX
              let log = unsplit_parts ["/home/v/log", zs ++ " tieflegen"]
              h <- openFile log AppendMode
              cwd <- getCurrentDirectory
              hPutStrLn h ("Arbeitsverzeichnis: " ++ quote cwd ++ "\n")

              outm "-04-"  --XX
              Main.ausfuehren 
                 zerleger
                 verschieben
                 (\pfad verzteile neudn -> do
                      -- die neuen Verzeichnisse anlegen
                      mapM_ (\verz -> (mkdir (unslice_path verz))
                                         `catch` (\ioe -> if isAlreadyExistsError ioe then return () else ioError ioe)
                            )
                            (tail (inits verzteile))

                      -- die Datei verschieben
                      let zieldn = if verschieben then unslice_path (verzteile ++ [filename_part pfad])
                                                  else unslice_path (verzteile ++ [neudn])
                      ex <- do ex <- path_exists zieldn
                               if ex then do rp_zieldn <- realpath zieldn
                                             rp_pfad   <- realpath pfad
                                             return (rp_zieldn /= rp_pfad)
                                     else return False
                      if ex && (not umbenennen)
                         then errm (quote zieldn ++ " gibt es schon.")
                         else do pfad_echt   <- realpath' pfad
                                 zieldn_echt <- realpath' zieldn
                                 when (pfad_echt /= zieldn_echt) $ do
                                    zieldn' <- verschmelzen_n Nothing ebenen umbenennen pfad zieldn
                                    when (pfad /= zieldn') $ do
                                       hPutStrLn h ("   " ++ quote pfad ++ "\n-> " ++ quote zieldn' ++ "\n")
                                       hFlush h

                 )
                 pfade
                 
              outm "-05-"  --XX
              hClose h
              return ()


-- Pfad zerlegen und die Bestandteile des neuen Pfades bestimmen. 

-- Dateinameanteil eines Pfades in Scheiben schneiden. Der ganze Verzeichnisteil kommt zusätzlich zur ersten Scheibe. Wenn flach, dann
-- kommt der erste Dateinamensteil zum Verzeichnisteil. Der Rest wird mit Trennern zusammengeklebt. Wenn nach Trenner getrennt wurde, kommt wieder das
-- gleiche dabei heraus. Wenn nicht (sondern mit reg. Ausdr.), dann etwas anderes. I.d.R. wird bei flach ein reg. Ausdr. aber auch nur zwei Teile
-- auszeichnen, dann spielt das keine Rolle.

pfad_zerlegen :: Zerleger
              -> String                         -- Trenner
              -> Int                            -- So viele Ebenen tief ausrollen
              -> Bool                           -- Ob Dateien verschieben, d.h. Dateiname nicht antasten
              -> String                         -- Pfad
              -> ( String                       -- Verzeichnis
                 , [String]                     -- neue Verzeichnisse
                 , String                       -- neuer Dateiname (das ist wieder der alte bei --verschieben)
                 )
pfad_zerlegen zerleger trenner tiefe verschieben pfad =
   let (verz, dn) = 
          split_path pfad

       (verzteile, dnteil) =
          if verschieben
             then (zerleger verschieben dn, dn)
             else case zerleger verschieben dn of
                     []    -> ([], dn)
                     teile -> letzten_ab teile

       verzteile_ausrollen = take tiefe verzteile
       verzteile_behalten  = drop tiefe verzteile

   in  ( verz
       , if spalten && not (null verzteile_ausrollen) 
            then [concat (intersperse trenner verzteile_ausrollen)]
            else verzteile_ausrollen
       , concat (intersperse trenner (verzteile_behalten ++ [dnteil]))
       ) 


-- Pfade zerlegen, d.h. Bestandteile der Dateinamen erkennen, und etwas damit machen.
ausfuehren :: Zerleger
           -> Bool                              -- ob Dateien nur verschieben (und nicht umbenennen)
           -> (   String                        -- Pfad
               -> [String]                      -- erkannte Verzeichnisteile
               -> String                        -- erkannter Dateinameteil
               -> IO a                          -- Ergebnis wird durchgereicht an Aufrufer. Kann Weiter werfen, um nichts zurückzugeben.
              )
           -> [String]                          -- Pfade
           -> IO [a]
ausfuehren zerleger verschieben aktion pfade = do
   erg <-
      foldM (\erg pfad ->
                do let (verz, verzteile, dnteil) = pfad_zerlegen zerleger trenner tiefe verschieben pfad
                   when (any null (dnteil : verzteile)) $ do
                      errm $ "Zerlegung liefert leeren Teil. Übersprungen.\n\
                             \   Dateiname: " ++ quote (filename_part pfad) ++ "\n\
                             \   Zerlegung: " ++ show verzteile ++ " - " ++ show dnteil
                      throw Weiter
                   e_logm ("Zerlegung von " ++ quote pfad ++ " :  " ++ show verzteile ++ ",  " ++ quote dnteil)
                   erg1 <- aktion pfad verzteile dnteil
                   return (erg1 : erg)

                `catch` (\Weiter -> return erg)
            )
            []
            pfade
   return (reverse erg)




------------------------------------------------------------------------------------------------------------------------------------------------------
-- Die Zerleger
--
-- Werden auf den Dateiname, ohne Verzeichnisteil, mit Erweiterung, angewendet. Liefern alle vom Benutzer gemeinten Teile. Das sind die direkt
-- angegebenen, bzw. je nachdem ob --verschieben gewählt ist. Sie werden von pfad_zerlegen angewendet.
--
-- Die gemeinten Teile sind bei Verschieben die neuen Oberverzeichnisse, ohne Verschieben die neuen Verzeichnisse und der neue Dateiname.


type Zerleger =    Bool         -- ob die Datei nur verschoben werden soll
                -> String       -- Dateiname (ohne Pfad)
                -> [String]     -- gefundene Teile


-- Text anhand Trenner in Scheiben schneiden.
-- Liefert bei --verschieben alle Teile außer dem letzten, sonst alle.
trenner_zerleger :: String -> Zerleger
trenner_zerleger trenner verschieben txt =
   if verschieben then fst (letzten_ab zerlegung)
                  else zerlegung
   where
      trenner_zerleger' trenner txt =
         case teiltext trenner txt of
            Just (davor, danach) -> davor : trenner_zerleger' trenner danach
            Nothing              -> [txt]

      zerlegung = trenner_zerleger' trenner txt


-- Zerleger aus fertig übersetztem regulärem Ausdruck.
regex_zerleger :: Regex -> Zerleger
regex_zerleger re verschieben txt =
   case matchRegex re txt of
      Just teile -> case teile of
                       [] -> [txt]
                       t  -> t
      Nothing    -> []


-- Zerleger aus regulärem Ausdruck, der als String gegeben ist.
regex_zerleger_t :: String              -- Reg. Ausdr.
                 -> Bool                -- ob Großkleinschreibung beachten
                 -> Zerleger
regex_zerleger_t regex beachten =
   regex_zerleger (mkRegexWithOpts regex False beachten)



------------------------------------------------------------------------------------------------------------------------------------------------------
-- Allgemeines

-- Letztes Element abtrennen
letzten_ab :: [a] -> ([a], a)
letzten_ab l =
   let (z:ys) = reverse l
   in  (reverse ys, z)
