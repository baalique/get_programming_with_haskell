robot (name, attack, hp) = \message -> message (name, attack, hp)

killerRobot = robot ("Killer", 25, 200)

name (n, _, _) = n
attack (_, a, _) = a
hp (_, _, h) = h

getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHP aRobot = aRobot hp

setName aRobot newName = aRobot (\(n, a, h) -> robot (newName, a, h))
setAttack aRobot newAttack = aRobot (\(n, a, h) -> robot (n, newAttack, h))
setHP aRobot newHP = aRobot (\(n, a, h) -> robot (n, a, newHP))

kittyRobot = setName killerRobot "Kitty"

printRobot aRobot = aRobot (\(n, a, h) -> n ++ " Attack: " ++ show a ++ " HP: " ++ show h)

damage aRobot attackDamage = aRobot (\(n, a, h) -> robot (n, a, h - attackDamage))

fight aRobot defender = damage defender attack where attack = if getHP aRobot > 10 then getAttack aRobot else 0

friendlyRobot = robot ("Friendly", 10, 300)

friendlyRound1 = fight killerRobot friendlyRobot
killerRound1 = fight friendlyRobot killerRobot
friendlyRound2 = fight killerRound1 friendlyRound1
killerRound2 = fight friendlyRound1 killerRound1
friendlyRound3 = fight killerRound2 friendlyRound2
killerRound3 = fight friendlyRound2 killerRound2

getHPFromList = map getHP
