import Text.Printf

type Point     = (Float,Float)
type Rect      = (Point,Float,Float)
type RGB       = (Float, Float, Float)

-- Gera retangulo SVG 
-- a partir de coordenadas+dimensoes e de uma string com atributos de estilo
writeRect :: (RGB,Rect) -> String 
writeRect ((r, g, b),((x,y),w,h)) = 
  printf "<rect x='%.3f' y='%.3f' width='%.2f' height='%.2f' style='stroke:black;stroke-width:2;fill:rgb(%.0f,%.0f,%.0f)' />\n" x y w h r g b

-- Gera codigo-fonte de arquivo SVG 
-- concatenando uma lista de retangulos e seus atributos de estilo
writeRects :: Float -> Float -> [(RGB,Rect)] -> String 
writeRects w h rs = 
  printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 
    ++ (concatMap writeRect rs) ++ "</svg>"


geraRects :: Float -> Float -> Float -> Float -> [Rect]
geraRects lin col w h =
  [((x, y), w, h)| y <- [0, h..(h*(lin-1))], x <- [0, w..(w*(col-1))]]
  
geraSL :: Float -> Float -> Float -> [(Float, Float, Float)]
geraSL hue lin col = 
  [(hue, s, l) | l <- [0, (1/(lin-1))..1], s <- [0, (1/(col-1))..1]]
  
geraRGB :: [(Float, Float, Float)] -> [(Float, Float, Float)]
geraRGB hsl = map(toRGB)hsl
  
geraPallete :: Float -> (Float, Float) -> (Float, Float) -> [(RGB, Rect)]
geraPallete hue (l,c) (w, h) =
  zip (geraRGB (geraSL hue l c)) (geraRects l c w h)
 
toRGB :: (Float, Float, Float) -> (Float, Float, Float)
toRGB (_, 0, l) = (l*255, l*255, l*255)
toRGB (h, s, l) = if(l<0.5)
  then (255 * (huergb (2 * l - (l*(1+s))) (l*(1+s)) (h + (1/3))), 255 * (huergb (2 * l - (l*(1+s))) (l*(1+s)) h), 255 * (huergb (2 * l - (l*(1+s))) (l*(1+s)) (h - (1/3))))
  else (255 * (huergb (2 * l - ((l+s) - (s*l))) ((l+s) - (s*l)) (h + (1/3))), 255 * (huergb (2 * l - ((l+s) - (s*l))) ((l+s) - (s*l)) h), 255 * (huergb (2 * l - ((l+s) - (s*l))) ((l+s) - (s*l)) (h - (1/3))))

huergb :: Float -> Float -> Float -> Float
huergb v1 v2 vh 
  |vh < 0 = huergb v1 v2 (vh+1)
  |vh > 1 = huergb v1 v2 (vh-1)
  |otherwise = huergb2 v1 v2 vh
  
huergb2 :: Float -> Float -> Float -> Float
huergb2 v1 v2 vh  
  |(vh*6) < 1 = v1 + ( v2 - v1 ) * 6 * vh
  |(vh*2) < 1 = v2
  |(vh*3) < 2 = v1 + ( v2 - v1 ) * ( ( 2 / 3 ) - vh ) * 6
  |otherwise = v1


  
main :: IO ()
main = do
  let
    hue = 0
    lin = 9.0
    col = 5.0
    w = 50.0
    h = 50.0
    width = col * w
    height = lin * h
    rects = geraPallete hue (lin,col) (w, h)
  writeFile "colors.svg" $ writeRects width height rects
  -- o codigo acima eh equivalente a:
  -- writeFile "colors.svg" (writeRects w h rects)
