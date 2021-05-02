estanRelacionados :: Float -> Float -> Bool
estanRelacionados a b | a <= 3 && b <= 3 = True
                      | (a > 3 && b > 3) && (a <= 7 && b <= 7) = True
                      | a > 7 && b > 7 = True
                      | otherwise = False

-- | Verifica que las 2 coords del 1er vector son menores que las del 2º
todoMenor (x1, y1) (x2, y2) = x1 < x2 && y1 < y2