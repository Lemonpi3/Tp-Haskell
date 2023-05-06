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

--Funciones Utils Creadas para resolver los ejercicios

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
                                

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red = existeEnLista "Roberto Carlos" (nombresDeUsuarios red)

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe = undefined

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA = undefined

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined

