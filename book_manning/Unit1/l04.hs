import Data.List

ifEven f x = if even x then f x else x
ifEvenCube x = ifEven (\x -> x^3) x

names = [("Ian", "Curtis"),
         ("Bernard","Sumner"),
         ("Alan","Sumner"),
         ("Peter", "Hook"),
         ("Stephen","Morris")]

compareLastNamesOriganal name1 name2 = if lastName1 > lastName2
                               then GT
                               else if lastName1 < lastName2
                                    then LT
                                    else if firstName1 > firstName2
                                        then GT
                                        else if firstName1 < firstName2
                                            then LT
                                            else EQ                                
  where lastName1 = snd name1
        lastName2 = snd name2
        firstName1 = fst name1
        firstName2 = fst name2

compareLastNames name1 name2 = if lastNameComparison /= EQ
                               then lastNameComparison
                               else compare firstName1 firstName2                                                        
  where lastName1 = snd name1
        lastName2 = snd name2
        firstName1 = fst name1
        firstName2 = fst name2
        lastNameComparison = compare lastName1 lastName2

addressLetter name location = locationFunction name
        where locationFunction = getLocationFunction location

getLocationFunction location = case location of          
        "ny" -> nyOffice                                       
        "sf" -> sfOffice                                       
        "reno" -> renoOffice
        "dc" -> dcOffice
        _ -> (\name -> fst name ++ " " ++ snd name)

sfOffice name = if lastName < "L"
                then nameText
                     ++ " - PO Box 1234 - San Francisco, CA, 94111"
                else nameText
                     ++ " - PO Box 1010 - San Francisco, CA, 94109"
  where lastName = snd name
        nameText = fst name ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
  where nameText = fst name ++ " " ++ snd name

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
  where nameText = snd name

dcOffice name = fst name ++ " " ++ snd name ++ " Esq"