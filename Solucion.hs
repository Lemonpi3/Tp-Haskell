module Solucion where
-- Completar con los datos del grupo
--
-- Nombre de Grupo: xx
-- Integrante 1: Agustin Lehmann, 96agustin.lehmann@gmail.com, 617/23
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

estaRepetido :: Eq t => t -> [t] -> Bool
--Revisa si esta duplicado en la lista
estaRepetido _ [] = False
estaRepetido t (x:xs) | t == x = True
                      | otherwise = estaRepetido t xs

borrarDuplicados :: Eq t => [t] -> [t]
--Retorna una lista sin duplicados
borrarDuplicados [] = []
borrarDuplicados (x:xs) | (estaRepetido x xs) = borrarDuplicados xs
                        | (estaRepetido x xs) == False = [x] ++ borrarDuplicados xs

existeLaRelacion :: RedSocial -> Usuario -> Usuario -> Bool
--se fija si existe la relacion entre usuarios (sin importar el orden) dentro de la red.
existeLaRelacion red usr1 usr2 = existeEnLista (usr1,usr2) (relaciones red) || existeEnLista (usr2,usr1) (relaciones red)

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
esMaximo _ [] = False
esMaximo n (x:xs) | n < x = False
                  | n >= x && xs /= [] = esMaximo n xs
                  | n >= x && xs == [] = True
                  | otherwise = True

existeEnLista :: Eq t => t -> [t] -> Bool
--Se fija si el obj existe o no en la lista
existeEnLista _ [] = False
existeEnLista item (x:xs) | item == x = True
                          | otherwise  = existeEnLista item xs

leDioLike :: Publicacion -> Usuario -> Bool
--devuelve si el usuario le dio like a la publicación.
leDioLike pub usr = existeEnLista usr (likesDePublicacion pub)


-----------------------------------------------------------------
-- Ejercicios

--1
nombresDeUsuarios :: RedSocial -> [String]
--Usa la funcion usuarios para obtener la lista de usuarios y parcea los nombres con extraerUsuarios.
nombresDeUsuarios red = extraerNombresDeUsuarios(usuarios red)
    where
        extraerNombresDeUsuarios :: [Usuario] -> [String]
        --Parcea los nombres de la lista de usuarios.
        extraerNombresDeUsuarios [] = []
        extraerNombresDeUsuarios (x:xs) = [nombreDeUsuario x] ++ extraerNombresDeUsuarios xs

--2
amigosDe :: RedSocial -> Usuario -> [Usuario]
--Devuelve una lista con los amigos del usuario de la Red Social dada.
amigosDe red usr = borrarDuplicados(listaDeAmigos usr (relaciones red))
    where
        listaDeAmigos :: Usuario -> [Relacion] -> [Usuario] 
        --Devuelve la lista de usuarios donde el usuario en cuestion esta en relacion con los otros, basado en la lista de relaciones dadas.
        listaDeAmigos _ [] = []
        listaDeAmigos usr (x:xs) | (existeEnLaRelacion usr x) = [extraerOtroUsuarioDeRel usr x] ++ listaDeAmigos usr xs
                                 | (existeEnLaRelacion usr x == False) = listaDeAmigos usr xs

        existeEnLaRelacion :: Usuario -> Relacion -> Bool
        --Se fija si el usuario pertenece a la relacion
        existeEnLaRelacion usr (x,xs) | usr == x || usr == xs = True
                                      | otherwise = False

--3
cantidadDeAmigos :: RedSocial -> Usuario -> Int
--Devuelve la cantidad de amigos que tiene un usuario en la red social
cantidadDeAmigos red usr = sizeLista(amigosDe red usr)

--4    
usuarioConMasAmigos :: RedSocial -> Usuario
--Devuelve el usuario con mas amigos de una red social, si hay empate devuelve el primero encontrado.
usuarioConMasAmigos red = usuarioMaxAmigos red (usuarios red)
    where
        cantidadesDeAmigos :: RedSocial -> [Usuario] -> [Int]
        --Util function: Crea una lista con la cantidad de amigos de cada usuario en la red.
        cantidadesDeAmigos _ [] = []
        cantidadesDeAmigos red (x:xs) = [cantidadDeAmigos red x] ++ cantidadesDeAmigos red xs

        usuarioMaxAmigos :: RedSocial -> [Usuario] -> Usuario
        --Util function: Dada una red y una lista de usuarios devuelve al usuario con mas amigos.
        usuarioMaxAmigos red (x:xs) | (esMaximo (cantidadDeAmigos red x) (cantidadesDeAmigos red xs)) == False && sizeLista xs > 0 = usuarioMaxAmigos red xs
                                    | otherwise = x

--5
estaRobertoCarlos :: RedSocial -> Bool
--Si hay un usuario con 10 o mas amigos en la red (segun correcion de la catedra en v2.1.1) retornara verdadero.
--Use el usuario con mas amigos ya que ese es el que mas chances tiene de serlo.
estaRobertoCarlos red | (cantidadDeAmigos red (usuarioConMasAmigos red)) > 10 = True
                      | otherwise = False

--6
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
--devuelve una lista de las publicaciones de una red.En las cuales el usuario en cuestion es el dueño de estas.
publicacionesDe red usr = extraerPublicaciones (publicaciones red) usr
    where
        extraerPublicaciones :: [Publicacion] -> Usuario -> [Publicacion]
        --Util: extrae las publicaciones de un usuario de una lista de publicaciones.
        extraerPublicaciones [] _ = []
        extraerPublicaciones (x:xs) usr | (usuarioDePublicacion x) == usr = [x] ++ extraerPublicaciones xs usr
                                        | otherwise = extraerPublicaciones xs usr

--7
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
--Devuelve una lista de las publicaciones de una red que le dio like el usuario.
publicacionesQueLeGustanA red usr = publicacionesConLikesDe (publicaciones red) usr
    where
        publicacionesConLikesDe :: [Publicacion] -> Usuario -> [Publicacion]
        --Dada una lista de publicaciones devuelve otra lista que contiene las publicaciones a las cuales el usuario le dio like.
        publicacionesConLikesDe [] _ = []
        publicacionesConLikesDe (x:xs) usr | leDioLike x usr = [x] ++ publicacionesConLikesDe xs usr
                                           | otherwise = publicacionesConLikesDe xs usr

--8
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
--Se fija si ambos usuarios tengan los mismos likes.
lesGustanLasMismasPublicaciones red usr1 usr2 = (publicacionesQueLeGustanA red usr1) == (publicacionesQueLeGustanA red usr2)

--9
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
--Revisa los likes los usuarios de la primera publicacion (ya que por especificacion un seguidor fiel tiene que darle like a todas) y se
--fija que exista almenos un usuario que le haya dado like a todas las publicaciones del usuario de Input en la red social. 
tieneUnSeguidorFiel red usr = haySeguidorFiel red (publicacionesDe red usr) (usuariosLikeDeLaPrimeraPub (publicacionesDe red usr))
    where
        usuariosLikeDeLaPrimeraPub :: [Publicacion] -> [Usuario]
        --dada una lista de publicaciones, devuelve los usuarios que le dieron like a la primera. (no sabia si podia usar head, sino usaba eso)
        usuariosLikeDeLaPrimeraPub (x:xs) = likesDePublicacion x

        leGustaLasPublicaciones :: [Publicacion] -> [Publicacion] -> Bool
        --Toma las publicaciones del usuario y las publicaciones del que le dio like.
        --se fija si estan las publicaciones en la lista de los likes, si todas estas significa que al usuario de los like le gusto todas las publicaciones.
        leGustaLasPublicaciones [] pubLikes = True
        leGustaLasPublicaciones (x:xs) pubLikes | existeEnLista x pubLikes = leGustaLasPublicaciones xs pubLikes
                                                | otherwise = False

        haySeguidorFiel :: RedSocial -> [Publicacion] -> [Usuario] -> Bool
        --revisa si existe un usuario que le dio likes a todas las publicaciones, si no lo encuentra retorna falso.
        haySeguidorFiel red pubs [] = False
        haySeguidorFiel red pubs (u:us) | leGustaLasPublicaciones pubs (publicacionesQueLeGustanA red u) = True
                                        | otherwise = haySeguidorFiel red pubs us

--10
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
--revisa si existe la relacion entre los usuarios, si no existe revisa los amigos del usr1 si tienen relacion con el usuario2 de forma recursiva.
--Osea, si estos tampoco tienen, revisa los amigos de los amigos.
existeSecuenciaDeAmigos red usr1 usr2 | usr1 == usr2 = False
                                      | existeLaRelacion red usr1 usr2 = True
                                      | otherwise = revisarAmigos red (amigosDe red usr1) usr2 []
    where
        revisarAmigos :: RedSocial -> [Usuario] -> Usuario -> [Usuario] -> Bool
        --se fija si algun usuario de la lista es amigo del usuario en cuestion.
        revisarAmigos _ [] _ _ = False
        revisarAmigos red (x:xs) usr visitados | existeEnLista x visitados = revisarAmigos red xs usr visitados
                                               | existeLaRelacion red x usr = True
                                               | revisarAmigos red (amigosDe red x) usr (x:visitados)= True
                                               | otherwise = revisarAmigos red xs usr visitados
