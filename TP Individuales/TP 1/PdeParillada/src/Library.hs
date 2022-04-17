module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero
{- ALUMNO
    Lucas Pangaro
    lpangaro@frba.utn.edu.ar
    Legajo: 164.142-6
-}    

{- ENUNCIADO
1. Modelar un grupo que asiste a un evento, del cual nos interesa el nombre del encargado del grupo, su edad, y las edades de los acompañantes (que puede haber cualquier cantidad).
2. Codificar una función que dado un grupo devuelva su cantidad de integrantes.
Ejemplo: si tenemos el grupo de “pedro” de 20 años con 2 acompañantes de 18 y 16 años de edad, su cantidad de integrantes es 3.
3. Codificar lo necesario para que, dados dos grupos, poder averiguar el nombre del encargado del grupo de mayor promedio de edad.
Ejemplo: si tenemos el grupo de “pedro” visto anteriormente y el grupo de “rosa” de 19 años con 1 acompañante de 21 años, cuando le pase esos dos grupos a la función debe devolver “rosa”, porque el promedio de edades del grupo de rosa es 20 mientras que el de pedro es 18.
-}

data Grupo = UnGrupo String [Number] deriving Show

-- GETTERS
nombre :: Grupo -> String
edades :: Grupo -> [Number]
nombre (UnGrupo nombre _) = nombre
edades (UnGrupo _ edades) = edades

-- 1
grupa_1 :: Grupo
grupa_1 = UnGrupo "Pedro" [20, 18, 16]
grupa_2 = UnGrupo "Maria" [19, 21]
grupa_3 = UnGrupo "Lucas" [18, 20, 16, 18]

-- 2
cant_integrantes :: Grupo -> Number
cant_integrantes grupo = length (edades grupo)
{-  TERMINAL:
    *Spec Library Spec> cant_integrantes grupa_1
    3
    *Spec Library Spec> cant_integrantes grupa_2
    2
    *Spec Library Spec> cant_integrantes grupa_3
    4 
-}

-- 3
promedio :: Grupo -> Number
promedio grupo = sum(edades grupo) / cant_integrantes grupo
{- TERMINAL:
    *Spec Library Spec> promedio grupa_1
    18
    *Spec Library Spec> promedio grupa_2
    20
    *Spec Library Spec> promedio grupa_3
    18
-}
grupo_mayor_promedio :: Grupo -> Grupo -> String
grupo_mayor_promedio grupo_1 grupo_2
        | promedio grupo_1 > promedio grupo_2 = nombre grupo_1
        | promedio grupo_2 > promedio grupo_1 = nombre grupo_2
        | otherwise = "Ambos grupos tienen el mismo promedio de edad"
{- TERMINAL:
    *Spec Library Spec> grupo_mayor_promedio grupa_1 grupa_2
    "Maria"
    *Spec Library Spec> grupo_mayor_promedio grupa_3 grupa_2
    "Maria"
    *Spec Library Spec> grupo_mayor_promedio grupa_3 grupa_1
    "Ambos grupos tienen el mismo promedio de edad"
-}