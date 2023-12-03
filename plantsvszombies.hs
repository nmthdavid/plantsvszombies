--Németh Dávid FWDBG4 Nagybeadandó feladat
--Plants vs Zombies 

import Data.List()
import Data.Maybe(isJust)
import Data.Maybe(isNothing)

--Típusok definíciója
type Coordinate = (Int, Int)
type Sun = Int

--Növények létrehzoása
-- name, hp
data Plant = Peashooter Int | Sunflower Int | Walnut Int | CherryBomb Int deriving (Show,Eq)

--Zombiek létrehozása
-- name, hp, speed
data Zombie = Basic Int Int | Conehead Int Int | Buckethead Int Int | Vaulting Int Int deriving (Show,Eq)

--Modell létrehozása
data GameModel = GameModel Sun [(Coordinate, Plant)] [(Coordinate, Zombie)] deriving (Show,Eq)

--Példák a feladathoz

defaultPeashooter :: Plant
defaultPeashooter = Peashooter 3

defaultSunflower :: Plant
defaultSunflower = Sunflower 2

defaultWalnut :: Plant
defaultWalnut = Walnut 15

defaultCherryBomb :: Plant
defaultCherryBomb = CherryBomb 2

basic :: Zombie
basic = Basic 5 1

coneHead :: Zombie
coneHead = Conehead 10 1

bucketHead :: Zombie
bucketHead = Buckethead 20 1

vaulting :: Zombie
vaulting = Vaulting 7 2

--Alapfeladat
--Növények vásárlása
{-
Peashooter-nek 100 Nap,
Sunflower-nek és Walnut-nak 50 Nap
CherryBomb-nak 150 Nap.
-}

--1.feladat
getPrice :: Plant -> Int
getPrice (Peashooter hp) = 100
getPrice (Sunflower hp) = 50
getPrice (Walnut hp) = 50
getPrice (CherryBomb hp) = 150

juste :: (Eq b, Eq a) => a -> [(a, b)] -> Bool
juste t1 t2 | lookup t1 t2 == Nothing = True
            | otherwise = False

tryPurchase :: GameModel -> Coordinate -> Plant -> Maybe GameModel
tryPurchase (GameModel sun plants zombies) (c1,c2) plant | c1 > 4 || c1 < 0 || c2 < 0 || c2 > 11 = Nothing
                                                         | juste (c1,c2) plants == False = Nothing
                                                         | getPrice plant > sun = Nothing
                                                         | otherwise = Just (GameModel (sun-(getPrice plant)) (plants++[((c1,c2),plant)]) (zombies))
--Sikeres tesztesetek

--2.feladat
getZombieName :: Zombie -> String 
getZombieName (Basic a b) = "basic"
getZombieName (Conehead a b) = "conehead"
getZombieName (Buckethead a b) = "buckethead"
getZombieName (Vaulting a b) = "vaulting"


placeZombieInLane :: GameModel -> Zombie -> Int -> Maybe GameModel
placeZombieInLane (GameModel sun plants zombies) zombie row | row > 4 || row < 0 = Nothing
                                                            | juste (row,11) zombies == False = Nothing
                                                            | otherwise = Just (GameModel sun plants ([((row,11),zombie)]++zombies))

--Sikeres tesztesetek

--3.feladat

--A növények életének csökkentése
damagePlant :: Plant -> Plant
damagePlant (Peashooter hp) = (Peashooter (hp-1))
damagePlant (Sunflower hp) = (Sunflower (hp-1))
damagePlant (Walnut hp) = (Walnut (hp-1))
damagePlant (CherryBomb hp) = (CherryBomb (hp-1))

--eldönti hogy a zombinak van-e még kettes gyorsasága
running :: (Coordinate,Zombie) -> Int
running (a,Vaulting b 2) = 2
running (a, _) = 1

--csökkenti a zombik sebességét
vaultDecrease :: [Zombie] -> [Zombie]
vaultDecrease [] = []
vaultDecrease ((Vaulting hp s):xs) = [Vaulting hp (s-1)] ++ vaultDecrease xs
vaultDecrease _ = []
         
performZombieActions :: GameModel -> Maybe GameModel
performZombieActions (GameModel s [] []) = Just(GameModel s [] [])
performZombieActions (GameModel s (p:ps) []) = Just(GameModel s (p:ps) [])

performZombieActions (GameModel s (ps) ((firstZombie:zs))) = case performZombieActions (GameModel s (ps) zs) of
-- ha már nem jó akkor nem foglalkozunk vele
    Nothing -> Nothing

    Just (GameModel sun plants zombies)
        | juste (fst zc, snd zc) plants == True && (snd zc) - running firstZombie < 0 -> Nothing
        | juste (fst zc, snd zc) plants == True && running firstZombie == 2 && juste (fst zc, (snd zc)-1) plants -> Just (GameModel sun plants (((fst zc, (snd zc)-2), head (vaultDecrease [z])) : zombies))
        | juste (fst zc, snd zc) plants == True && otherwise -> Just (GameModel sun plants (((fst zc, (snd zc)-running firstZombie), z) : zombies))

        | juste (fst zc, snd zc) plants == False && running firstZombie == 1 -> Just (GameModel sun (coordinateDamage plants) (((fst zc, (snd zc)), z) : zombies))
        | juste (fst zc, snd zc) plants == False && otherwise -> Just  (GameModel sun plants (((fst zc, (snd zc)-1),head (vaultDecrease [z])) : zombies))

        where 
            zc = fst firstZombie
            z = snd firstZombie

            coordinateDamage :: [(Coordinate, Plant)] -> [(Coordinate, Plant)]
            coordinateDamage [] = []
            coordinateDamage (x:xs)
                | fst x == (fst zc, (snd zc)) = [(fst x, damagePlant (snd x))]++ coordinateDamage xs
                | otherwise = [(fst x, snd x)] ++ coordinateDamage xs

--4.feladat
isAliveZombie :: (Coordinate,Zombie) -> Bool
isAliveZombie ((c1,c2),(Basic hp sp)) = hp>0
isAliveZombie ((c1,c2),(Conehead hp sp)) = hp>0
isAliveZombie ((c1,c2),(Buckethead hp sp)) = hp>0
isAliveZombie ((c1,c2),(Vaulting hp sp)) = hp>0

isAlivePlant :: (Coordinate,Plant) -> Bool
isAlivePlant ((c1,c2),(Peashooter hp)) = hp>0
isAlivePlant ((c1,c2),(Sunflower hp)) = hp>0
isAlivePlant ((c1,c2),(Walnut hp)) = hp>0
isAlivePlant ((c1,c2),(CherryBomb hp)) = hp>0

filterZombies :: [(Coordinate,Zombie)] -> [(Coordinate,Zombie)]
filterZombies zombies = filter isAliveZombie zombies

filterPlants :: [(Coordinate,Plant)] -> [(Coordinate,Plant)]
filterPlants plants  = filter isAlivePlant plants

cleanBoard :: GameModel -> GameModel
cleanBoard (GameModel sun plants zombies) = (GameModel sun (filterPlants plants) (filterZombies zombies))