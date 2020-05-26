patientInfo' :: String -> String -> Int -> Int -> String
patientInfo' fname lname age height = name ++ " " ++ ageHeight
 where name = lname ++ ", " ++ fname
       ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

type FirstName = String
type LastName = String
type Age = Int
type Height = Int
type PatientName = (String,String)

firstName :: PatientName -> String
firstName = fst 
lastName :: PatientName -> String
lastName = snd

patientInfo :: PatientName -> Age -> Height -> String
patientInfo (fname,lname) age height = name ++ " " ++ ageHeight
 where name = lname ++ ", " ++ fname
       ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

type MiddleName = String
data Name = Name FirstName LastName
            | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

data Sex = Male | Female

data RhType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType -- data ctor combine 

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"
showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"
patient1BT = BloodType A Pos
showBloodType (BloodType abo rh)  = showABO abo ++ showRh rh

data Patient' = Patient' Name Sex Int Int Int BloodType
johnDoe :: Patient'
johnDoe = Patient' (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

jane = Patient' (NameWithMiddle "Jane" "Elizabeth" "Smoth") Female 40 69 200 (BloodType O Neg)

data Patient = Patient { name :: Name
                       , sex :: Sex
                       , age :: Int
                       , height :: Int
                       , weight :: Int
                       , bloodType :: BloodType }
jackieSmith = Patient {name = Name "Jackie" "Smith"
                        , age = 43
                        , sex = Female
                        , height = 62
                        , weight = 115
                        , bloodType = BloodType O Neg }
oPatient = jackieSmith
aPatient = jackieSmith { bloodType = BloodType A Pos }
bPatient = jackieSmith { bloodType = BloodType B Pos }
abPatient = jackieSmith { bloodType = BloodType AB Pos }

{-canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True                      1
canDonateTo _ (BloodType AB _) = True                     2
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False --otherwise-}

canDonateTo :: Patient -> Patient -> Bool
canDonateTo (Patient _ _ _ _ _ (BloodType O _)) _ = True
canDonateTo _ (Patient _ _ _ _ _ (BloodType AB _)) = True
canDonateTo (Patient _ _ _ _ _ (BloodType A _)) (Patient _ _ _ _ _ (BloodType A _)) = True
canDonateTo (Patient _ _ _ _ _ (BloodType B _)) (Patient _ _ _ _ _ (BloodType B _)) = True
canDonateTo _ _ = False

sexLitteral :: Sex -> String
sexLitteral Male = "Male"
sexLitteral Female = "Female"

patientSummary :: Patient -> String
patientSummary (Patient name sex age height weight bloodype) = 
      "**************\n" ++
      "Patient name: " ++ showName name ++ "\n" ++
      "Sex: " ++ sexLitteral sex ++ "\n" ++
      "Age: " ++ show age ++ "\n" ++
      "Height: " ++ show height ++ " in.\n" ++
      "Weight: " ++ show weight ++ " lbs.\n" ++
      "Blood Type: " ++ showBloodType bloodype ++ ".\n" ++
      "**************\n"

