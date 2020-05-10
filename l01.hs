messyMain :: IO()
messyMain = do
   print "Who is the email for?"
   recipient <- getLine
   print "What is the Title?"
   title <- getLine
   print "Who is the Author?"
   author <- getLine
   print (createEmail recipient title author)

toPart recipient = "Dear and admirable " ++ recipient ++ ",\n"
bodyPart bookTitle = "Thanks for buying my too expensive " ++ bookTitle ++ ".\n"
fromPart author = "Thanks, please subscribe,\n"++author
createEmail recipient bookTitle author = toPart recipient ++
                                         bodyPart bookTitle ++
                                         fromPart author