genIfXEven x = (\f -> if even x then f x else x)

titi = genIfXEven 2
tata = genIfXEven 3

getRequestURL host apiKey resource id = host ++
                                        "/" ++
                                        resource ++
                                        "/" ++
                                        id ++
                                        "?token=" ++
                                        apiKey

genHostRequestBuilder host = (\apiKey resource id -> getRequestURL "http://example.com" apiKey resource id)
genApiRequestBuilder hostBuilder apiKey = (\resource id -> hostBuilder apiKey resource id)
genApiResourceBuilder hostBuilder apiKey resource = (\id -> hostBuilder apiKey resource id)

getBook =  getRequestURL "http://example.com" "1337hAsk3ll" "book"

flipBinaryArgs f x y = f y x
sub2 = flip (-) 2 

ifEven f x = if even x
             then f x
             else x

ifEvenInc = ifEven (+1)
ifEvenDouble = ifEven (*2)
ifEvenSquare = ifEven (^2)

binaryPartialApplication f x = (\y -> f x y)
inc1 = binaryPartialApplication (+) 1