module Solucionn where
-- Completar con los datos del grupo
--
-- Nombre de Grupo: xx
-- Integrante 1: Nombre Apellido, email, LU
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

---------------------------------------------------------------------------------

--Funciones Utils Generales Creadas para resolver los ejercicios

-- Verifica si una lista está vacía.
nulo :: [a] -> Bool
nulo [] =  True
nulo (_:_) =  False

-- Verifica si algún elemento de la lista cumple con la condición dada.
cualquier :: (a -> Bool) -> [a] -> Bool
cualquier _ [] = False
cualquier p (x:xs) = p x || cualquier p xs

-- Verifica si un elemento está presente en la lista.
pertenece :: Eq a => a -> [a] -> Bool
pertenece e l = cualquier (== e) l

-- Verifica si dos elementos son diferentes.
distinto :: Eq a => a -> a -> Bool
distinto x y = not (x == y)

-- Verifica si todos los elementos de la lista cumplen con la condición dada.
todos :: (a -> Bool) -> [a] -> Bool
todos _ [] = True
todos p (x:xs) = p x && todos p xs

-- Filtra una lista manteniendo solo los elementos que cumplen con la condición dada.
filtrar :: (a -> Bool) -> [a] -> [a]
filtrar _ [] = []
filtrar p (x:xs)  | p x = x : filtrar p xs
                  | otherwise = filtrar p xs

-- Verifica si dos listas contienen los mismos elementos, sin importar el orden.
mismosElementos :: Eq a => [a] -> [a] -> Bool
mismosElementos l1 l2 = sizeLista l1 == sizeLista l2 && todos (`pertenece` l2) l1 && todos (`pertenece` l1) l2

-- Verifica si una lista no contiene elementos repetidos.
sinRepetidos :: Eq a => [a] -> Bool
sinRepetidos [] = True
sinRepetidos (x:xs) = not (pertenece x xs) && sinRepetidos xs

-- Verifica si no hay IDs repetidos en una lista de usuarios.
noHayIdsRepetidos :: [Usuario] -> Bool
noHayIdsRepetidos [] = True
noHayIdsRepetidos (x:xs) = not (estaRepetido (idDeUsuario x) xs) && noHayIdsRepetidos xs
  where estaRepetido _ [] = False
        estaRepetido id (x:xs) = id == idDeUsuario x || estaRepetido id xs

-- Verifica si una lista está vacía.
esVacio :: [a] -> Bool
esVacio [] = True
esVacio _ = False

-- Verifica si un usuario es válido.
usuarioValido :: Usuario -> Bool
usuarioValido u = idDeUsuario u > 0 && not (esVacio (nombreDeUsuario u))

-- Verifica si todos los usuarios de una lista son válidos y no tienen IDs repetidos.
usuariosValidos :: [Usuario] -> Bool
usuariosValidos us = todos (\x -> usuarioValido x) us && noHayIdsRepetidos us

-- Verifica si los usuarios de una lista son válidos y están involucrados en todas las relaciones dadas.
usuariosDeRelacionValidos :: [Usuario] -> [Relacion] -> Bool
usuariosDeRelacionValidos _ [] = True
usuariosDeRelacionValidos us ((u1, u2):rs) = pertenece u1 us && pertenece u2 us && distinto u1 u2 && usuariosDeRelacionValidos us rs

-- Verifica si una relación es asimétrica en el contexto de una lista de relaciones.
esAsimetrica :: Relacion -> [Relacion] -> Bool
esAsimetrica (a, b) rels = not (pertenece (b, a) rels)

-- Verifica si todas las relaciones en una lista son asimétricas.
relacionesAsimetricas :: [Relacion] -> Bool
relacionesAsimetricas [] = True
relacionesAsimetricas rels = todos (`esAsimetrica` rels) rels

-- Verifica si no hay relaciones repetidas en una lista de relaciones.
noHayRelacionesRepetidas :: [Relacion] -> Bool
noHayRelacionesRepetidas [] = True
noHayRelacionesRepetidas (x:xs) =
    not (pertenece x xs) && noHayRelacionesRepetidas xs

-- Verifica si las relaciones dadas son válidas en función de una lista de usuarios.
relacionesValidas :: [Usuario] -> [Relacion] -> Bool
relacionesValidas us rels = usuariosDeRelacionValidos us rels && relacionesAsimetricas rels && noHayRelacionesRepetidas rels

-- Compara dos publicaciones verificando que los usuarios y los títulos sean diferentes.
compararPublicaciones :: Publicacion -> Publicacion -> Bool
compararPublicaciones (u,t,c) (v,w,_) = distinto (idDeUsuario u) (idDeUsuario v) && distinto t w

-- Verifica si no hay publicaciones repetidas en una lista de publicaciones.
noHayPublicacionesRepetidas :: [Publicacion] -> Bool
noHayPublicacionesRepetidas [] = True
noHayPublicacionesRepetidas (x:xs) =
    not (pertenece x xs) && noHayPublicacionesRepetidas xs

-- Verifica si todos los usuarios de los likes están en la lista de usuarios.
usuariosLikeValidos :: [Usuario] -> [Usuario] -> Bool
usuariosLikeValidos us usl = todos (`pertenece` us) usl

-- Verifica si los usuarios que dieron like en una publicación están en la lista de usuarios.
usuariosLikeValidosDePublicacion :: [Usuario] -> Publicacion -> Bool
usuariosLikeValidosDePublicacion us pub = usuariosLikeValidos us (likesDePublicacion pub)

-- Verifica si los usuarios que dieron like en todas las publicaciones están en la lista de usuarios.
usuariosDeLikeDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuariosDeLikeDePublicacionSonUsuariosDeRed us pubs = todos (usuariosLikeValidosDePublicacion us) pubs

-- Verifica si el usuario de una publicación está en la lista de usuarios.
usuarioPerteneceALista :: [Usuario] -> Publicacion -> Bool
usuarioPerteneceALista us pub = pertenece (usuarioDePublicacion pub) us

-- Verifica si todos los usuarios de las publicaciones están en la lista de usuarios.
usuariosDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuariosDePublicacionSonUsuariosDeRed us pubs = todos (usuarioPerteneceALista us) pubs

-- Verifica la validez de las publicaciones en función de la lista de usuarios.
publicacionesValidas :: [Usuario] -> [Publicacion] -> Bool
publicacionesValidas us pubs =  usuariosDePublicacionSonUsuariosDeRed us pubs &&  usuariosDeLikeDePublicacionSonUsuariosDeRed us pubs && noHayPublicacionesRepetidas pubs

-- Verifica si una red social es válida, es decir, si los usuarios, relaciones y publicaciones son válidos.
redSocialValida :: RedSocial -> Bool
redSocialValida red =  usuariosValidos (usuarios red) && relacionesValidas (usuarios red) (relaciones red) && publicacionesValidas (usuarios red) (publicaciones red)


estaRepetido :: Eq t => t -> [t] -> Bool
--Revisa si esta duplicado en la lista
estaRepetido _ [] = False
estaRepetido t (x:xs) | t == x = True
                      | otherwise = estaRepetido t xs

borrarDuplicados :: Eq t => [t] -> [t]
--Retorna una lista sin duplicados
borrarDuplicados [] = []
borrarDuplicados (x:xs) | (estaRepetido x xs)= borrarDuplicados xs
                        | (estaRepetido x xs) == False = [x] ++ borrarDuplicados xs
                        | otherwise = []

existeEnRelacion :: Usuario -> Relacion -> Bool
--Se fija si el usuario pertenece a la relacion
existeEnRelacion usr (x,xs) | usr == x || usr == xs = True
                            | otherwise = False

extraerOtroUsuarioDeRel :: Usuario -> Relacion -> Usuario
--Devuelve el otro usuario de la relacion
extraerOtroUsuarioDeRel usr (x,xs) | usr == x = xs
                                   | usr == xs = x


sizeLista :: [t] -> Int
--Devuelve el tamaño de una lista
sizeLista [] = 0
sizeLista (x:xs) = 1 + sizeLista xs

esMaximo :: Int -> [Int] -> Bool
--Se fija si el numero es el maximo de la lista de ints
esMaximo n (x:xs) | n < x = False
                  | n >= x && xs /= [] = esMaximo n xs
                  | n >= x && xs == [] = True
                  | otherwise = True

existeEnLista :: Eq t => t -> [t] -> Bool
--Se fija si el obj existe o no en la lista
existeEnLista item [] = False
existeEnLista item (x:xs) | item == x = True
                          | otherwise  = existeEnLista item xs

-----------------------------------------------------------------
-- Ejercicios

nombresDeUsuarios :: RedSocial -> [String]
--Usa la funcion usuarios para obtener la lista de usuarios y parcea los nombres con extraerUsuarios.
nombresDeUsuarios red = extraerNombresDeUsuarios(usuarios red)
    where
        extraerNombresDeUsuarios :: [Usuario] -> [String]
        --Parcea los nombres de la lista de usuarios.
        extraerNombresDeUsuarios (x:xs) | xs /= [] = [nombreDeUsuario x] ++ extraerNombresDeUsuarios xs
                                        | otherwise = [nombreDeUsuario x]

amigosDe :: RedSocial -> Usuario -> [Usuario]
--Devuelve una lista con los amigos del usuario de la Red Social dada.
amigosDe red usr = borrarDuplicados(listaDeAmigos usr (relaciones red))
    where
        listaDeAmigos :: Usuario -> [Relacion] -> [Usuario] 
        --Devuelve la lista de usuarios donde el usuario en cuestion esta en relacion con los otros, basado en la lista de relaciones dadas.
        listaDeAmigos usr (x:xs) | (existeEnRelacion usr x) && (xs /= []) = [extraerOtroUsuarioDeRel usr x] ++ listaDeAmigos usr xs
                                | (existeEnRelacion usr x) && (xs == []) = [extraerOtroUsuarioDeRel usr x]
                                | (existeEnRelacion usr x == False) && (xs /= []) = listaDeAmigos usr xs
                                | otherwise = []

cantidadDeAmigos :: RedSocial -> Usuario -> Int
--Devuelve la cantidad de amigos que tiene un usuario en la red social
cantidadDeAmigos red usr = sizeLista(amigosDe red usr)
    
usuarioConMasAmigos :: RedSocial -> Usuario
--Devuelve el usuario con mas amigos de una red social, si hay empate devuelve el primero encontrado.
usuarioConMasAmigos red= usuarioMaxAmigos red (usuarios red)
    where
        cantidadesDeAmigos :: RedSocial -> [Usuario] -> [Int]
        --Util function: Crea una lista con la cantidad de amigos de cada usuario en la red.
        cantidadesDeAmigos red [] = []
        cantidadesDeAmigos red (x:xs) = [cantidadDeAmigos red x] ++ cantidadesDeAmigos red xs

        usuarioMaxAmigos :: RedSocial -> [Usuario] -> Usuario
        --Util function: Dada una red y una lista de usuarios devuelve al usuario con mas amigos.
        usuarioMaxAmigos red (x:xs) | (esMaximo (cantidadDeAmigos red x) (cantidadesDeAmigos red xs)) == False = usuarioMaxAmigos red xs
                                    | otherwise = x

estaRobertoCarlos :: RedSocial -> Bool
--Si hay un usuario con 10 o mas amigos en la red (segun correcion de la catedra en v2.1.1) retornara verdadero.
--Use el usuario con mas amigos ya que ese es el que mas chances tiene de serlo.
estaRobertoCarlos red | (cantidadDeAmigos red (usuarioConMasAmigos red)) > 10 = True
                      | otherwise = False

-- describir qué hace la función: Obtiene las publicaciones asociadas a un usuario en una red social válida, verificando la validez de la red, el usuario y la ausencia de elementos repetidos en la lista resultante.
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red u =     
  let pubs = filtrar (\pub -> usuarioDePublicacion pub == u) (publicaciones red)
  in if sinRepetidos pubs then pubs else borrarDuplicados pubs

-- describir qué hace la función: Obtiene todas las publicaciones que le gustan a un usuario en una red social.
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red u =
    let pubs = filtrar (\pub -> pertenece u (likesDePublicacion pub)) (publicaciones red)
    in if sinRepetidos pubs then pubs else borrarDuplicados pubs

-- Comprueba si dos usuarios les gustan las mismas publicaciones en una red social.
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2=
    let pubs1 = publicacionesQueLeGustanA red u1
        pubs2 = publicacionesQueLeGustanA red u2
    in mismosElementos pubs1 pubs2

-- Comprueba si un usuario tiene un seguidor al que le gustan todas sus publicaciones en una red social.
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red u =
    let pubs = publicacionesDe red u
        seguidoresFieles = filter (\u2 -> todos (\pub -> pertenece u2 (likesDePublicacion pub)) pubs) (usuarios red)
    in not (nulo seguidoresFieles) && not (nulo pubs)

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined