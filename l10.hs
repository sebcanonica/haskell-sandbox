cup flOz = \message -> message flOz

getOz aCup = aCup (\flOz -> flOz)

drink aCup ozDrank = if ozDiff >= 0
                     then cup ozDiff
                     else cup 0
                where flOz = getOz aCup
                      ozDiff = flOz - ozDrank

isEmpty aCup = getOz aCup == 0

robot :: (String, Int, Int) -> ((String, Int, Int) -> t) -> t
robot (name,attack,hp)  = \message -> message (name,attack,hp)

-- name :: (String, Int, Int) -> String
name (n,_,_) = n
-- attack :: (String, Int, Int) -> Int
attack (_,a,_) = a
-- hp :: (String, Int, Int) -> Int
hp (_,_,hp) = hp
getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHP aRobot = aRobot hp

setName aRobot newName = aRobot (\(n,a,h) -> robot (newName,a,h))
setAttack aRobot newAttack = aRobot (\(n,a,h) -> robot (n,newAttack,h))
setHP aRobot newHP = aRobot (\(n,a,h) -> robot (n,a,newHP))

printRobot aRobot = aRobot (\(n,a,h) -> n ++
                                        " attack:" ++ show a ++
                                        " hp:"++ show h)

damage aRobot attackDamage = aRobot (\(n,a,h) -> robot (n,a,h-attackDamage))

fight aRobot defender = damage defender attack
  where attack = if getHP aRobot > 10
                 then getAttack aRobot
                 else 0

fastRobot = robot ("speedy", 15, 40)
slowRobot = robot ("slowpoke", 20, 30)                 

slowRobotRound1 = fight fastRobot slowRobot
fastRobotRound1 = fight slowRobotRound1 fastRobot
slowRobotRound2 = fight fastRobotRound1 slowRobotRound1
fastRobotRound2 = fight slowRobotRound2 fastRobotRound1
slowRobotRound3 = fight fastRobotRound2 slowRobotRound2
fastRobotRound3 = fight slowRobotRound3 fastRobotRound2


printAllHps = map getHP

returnWinner robotA robotB = robot(
  robotA ( \(nA,aA,hA) -> 
    robotB ( \(nB,aB,hB) -> 
      if hA > hB 
      then (nA, aA, hA)
      else (nB, aB, hB)
    )
  ))

bob = robot ("bob", 1, 100)
threeRobots = [bob, slowRobot, fastRobot]
bobby = robot ("bobby", 10, 150)

bobbyFight = fight bobby
victims = map bobbyFight threeRobots
lives = map getHP victims