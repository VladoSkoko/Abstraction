module Main where
import Graphics.UI.WX
import Data.Strings

charAt :: Int -> String -> Char
charAt indeks niska = strLast (strTake (indeks+1) niska)

indexOf_pom :: Char -> String -> Int -> Int  
indexOf_pom karakter niska n
    | ((strLen niska) == 0)
        = -1-n 
    | ((strHead niska) == karakter)
        = 0
    | otherwise
        = 1 + indexOf_pom karakter (strTail niska) n 

indexOf :: Char -> String -> Int
indexOf karakter niska = indexOf_pom karakter niska (strLen niska) 

oslobodiZagrade :: String -> String 
oslobodiZagrade term
    | (strLen term) == 0
        = "" 
    | ((strHead term) == '(') && ((charAt 2 term) == ')')
        = (charAt 1 term) : oslobodiZagrade(strDrop 3 term)
    | otherwise 
        = (strHead term) : oslobodiZagrade (strTail term)

skiniZagrade :: String -> String
skiniZagrade term
    | (strHead term) == '(' && (strLast term) == ')'
        = take ((strLen term)-2) (strTail term)
    | otherwise
        = term
        
razdelnik :: String -> Int -> Int -> Int 
razdelnik term zagrade i
 | ((zagrade == 0) && (i /= 0)) 
        = i
 | (charAt (strLen(term)-1) term) == ')' 
        = razdelnik (take (strLen(term)-1) term) (zagrade+1) (i+1)
 | (charAt (strLen(term)-1) term) == '(' 
        = razdelnik (take (strLen(term)-1) term) (zagrade-1) (i+1) 
 | otherwise 
        = razdelnik (take (strLen(term)-1) term) zagrade (i+1)        

apstrakcija :: String -> Char -> String
apstrakcija term promenljiva = 
    do  if ((strHead term) == '(') && ((strLast term) == ')')
            then apstrakcija (strTail (take (strLen (term)-1) term)) promenljiva
            else 
                if (((strLen term) == 1) && ((charAt 0 term) == promenljiva))
                    then 
                        do povr_vr <- "I"
                           return povr_vr
                    else if ((indexOf promenljiva term) == -1)
                        then 
                            do povr_vr <- (strAppend (strAppend ")" term) "K(") 
                               return povr_vr
                    else if ((indexOf promenljiva (take (strLen(term)-1) term)) == -1) && ((strLast term) == promenljiva)
                        then 
                            do povr_vr <- (take (strLen(term)-1) term)
                               return povr_vr
                    else 
                        let i = (razdelnik term 0 0)
                        in 
                            do povr_vr <- (strAppend
                                    (strAppend 
                                        (strAppend 
                                            (strAppend 
                                                ")" 
                                                (apstrakcija (strDrop (strLen(term)-i) term) promenljiva)
                                            ) 
                                            ")("
                                        )
                                        (apstrakcija (take (strLen(term)-i) term) promenljiva)
                                    )
                                    "S(")
                               return povr_vr

standardnaApstrakcija :: String -> String -> String 
standardnaApstrakcija nizPromenljivih term 
    | (strLen nizPromenljivih) == 0
        = strAppend term "\n" 
    | otherwise
        = let p = strLen(nizPromenljivih)-1
          in strAppend 
          (standardnaApstrakcija (take p nizPromenljivih) (oslobodiZagrade (apstrakcija term (charAt p nizPromenljivih))))
          (strAppend (strAppend (strAppend (strAppend term ".") ">") ((charAt p nizPromenljivih):"")) "\n<")
          
standardnaApstrakcijaFinal :: String -> String -> String 
standardnaApstrakcijaFinal nizPromenljivih term 
    | (strLen nizPromenljivih) == 0
        = term
    | otherwise
        = let p = strLen(nizPromenljivih)-1
          in standardnaApstrakcijaFinal (take p nizPromenljivih) (oslobodiZagrade (apstrakcija term (charAt p nizPromenljivih)))

razdelnikSpreda :: String -> Int -> Int -> Int
razdelnikSpreda term zagrade i
    | (charAt i term) /= '(' && zagrade == 0 && i == 0
        = i+1
    | i /= 0 && zagrade == 0
        = i
    | (charAt i term) == '('
        = razdelnikSpreda term (zagrade+1) (i+1)
    | (charAt i term) == ')' && zagrade /= 0
        = razdelnikSpreda term (zagrade-1) (i+1)
    | otherwise
        = razdelnikSpreda term zagrade (i+1)  
       
primena :: String -> String -> String
primena term nizPromenljivih = oslobodiZagrade (primenaPom (strAppend nizPromenljivih term) "")

primenaPom :: String -> String -> String
primenaPom term prethodniKarakter
    | term == ""
        = ""
    | (strHead term) == 'I'
        = let pom1 = razdelnikSpreda (strTail term) 0 0
              arg1 = take pom1 (strTail term)
          in primenaPom (strAppend (drop (1+pom1) term) (i (skiniZagrade arg1))) prethodniKarakter
    
    | (strHead term) == 'K'
        = let pom1 = razdelnikSpreda (strTail term) 0 0
              arg1 = take pom1 (strTail term)
              pom2 = razdelnikSpreda (drop pom1 (strTail term)) 0 0
              arg2 = take pom2 (drop pom1 (strTail term))
          in
            primenaPom (strAppend (drop (1+pom1+pom2) term) (k (skiniZagrade arg1) (skiniZagrade arg2))) prethodniKarakter
    
    | (strHead term) == 'S'
        = let pom1 = razdelnikSpreda (strTail term) 0 0
              arg1 = take pom1 (strTail term)
              pom2 = razdelnikSpreda (drop pom1 (strTail term)) 0 0
              arg2 = take pom2 (drop pom1 (strTail term))
              pom3 = razdelnikSpreda (drop (pom1+pom2) (strTail term)) 0 0
              arg3 = take pom3 (drop (pom1+pom2) (strTail term))
          in 
            primenaPom (strAppend (drop (1+pom1+pom2+pom3) term) (s (skiniZagrade arg1) (skiniZagrade arg2) (skiniZagrade arg3))) prethodniKarakter
    
    | otherwise
        = (strHead term):(primenaPom (strTail term) (strAppend ((strHead term):"") prethodniKarakter))

i :: String -> String
i x = x

k :: String -> String -> String
k x y = x

s :: String -> String -> String -> String
s x y z = strAppend (strAppend (strAppend ")" z) ('(':y)) (strAppend z x)

main :: IO ()
main
  = start gui 

gui :: IO ()
gui
  = do  f    <- frame    []
        p    <- panel  f []
        prom <- entry  p [Graphics.UI.WX.text := ""]
        izraz<- entry  p [Graphics.UI.WX.text := ""]
        rezA <- staticText  p [Graphics.UI.WX.text := "\n\n\n\n\n\n\n\n"]
        rezP <- staticText  p [Graphics.UI.WX.text := ""]
        ok   <- button p [Graphics.UI.WX.text := "Izvrsi apstrakciju"
                         ,on command := do  s <- get prom Graphics.UI.WX.text
                                            t <- get izraz Graphics.UI.WX.text
                                            set rezA [Graphics.UI.WX.text := standardnaApstrakcija s t]
                                            set rezP [Graphics.UI.WX.text := primena (standardnaApstrakcijaFinal s t) s]
                         ]
        quit <- button p [Graphics.UI.WX.text := "Prekini program", on command := close f]
        set f [Graphics.UI.WX.text := "Apstrakcija"
              ,bgcolor := white
              ,layout := minsize (sz 400 500) $ container p 
                                              $ margin 10 
                                              $ column 5 
                                             [floatCentre (label "Unesite promenljive:")
                                             ,floatCentre (hfill (widget prom))
                                             ,floatCentre (label "Unesite izraz:")
                                             ,floatCentre (hfill (widget izraz))
                                             ,floatCentre (hfill (widget ok))
                                             ,floatCentre (label "Rezultat apstrakcije:")
                                             ,floatCentre (hfill (widget rezA))
                                             ,floatCentre (label "Rezultat primene dobijenog kombinatora:")
                                             ,floatCentre (hfill (widget rezP))
                                             ,floatCentre (hfill (widget quit))
                                             ] 
              ]
