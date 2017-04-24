:- module(problems, [problem/2]).

% ADAPTED!!!
% Clue 4: the 'sometime' was dropped
problem(p1, problem(4,5, [
                        "The perens pig lives in Slovakia and was recognized as endangered in 2009",
                        "The eldar elk has a population size of 210",
                        "The byengo bat doesn't live in Ghana",
                        "The animal that lives in Slovakia was listed before the animal from Russia",
                        "Of the species with a population size of 490 and the eldar elk, one lives in Poland and the other was recognized as endangered in 2009",
                        "Neither the nibner newt nor the byengo bat has a surviving population size of 525",
                        "The species that lives in Ghana was listed 2 years after the nibner newt",
                        "The animal that lives in Russia doesn't have a surviving population size of 315"
        ], [
                        noun(species, [species], [species]),
                        noun(species, [animal], [animals]),
                        pn(species, [the, perens, pig]),
                        pn(species, [the, byengo, bat]),
                        pn(species, [the, nibner, newt]),
                        pn(species, [the, eldar, elk]),
                        pn(species, [the, osbele, oryx]),
                        pn(country, [ghana]),
                        pn(country, [honduras]),
                        pn(country, [poland]),
                        pn(country, [russia]),
                        pn(country, [slovakia]),
                        noun(year, [year], [years]),
                        ivpp(pred(species, country), [lives], [in], [live]),
                        ivpp(pred(species, year), [was, recognized, as, endangered], [in], [are, recognized, as, endangered]),
                        ivpp(pred(species, year), [was, listed], [in], [are, listed]),
                        ivpp(pred(species, population), [has, a, population, size], [of], [have, a, population, size]),
                        ivpp(pred(species, population), [has, a, surviving, population, size], [of], [have, a, surviving, population, size]),
                        prep(fun(country, species), [from]),
                        prep(fun(population, species), [with, a, population, size, of])
        ])).

% ADAPTED!!!
% dropped "somewhat" from clue 4
problem(p2, problem(4,5, [
                        "Of the contestant who scored 41 points and the person who threw the white darts, one was from Worthington and the other was Ira",
                        "Bill was from Mount union",
                        "Ira scored 21 points higher than the contestant from Worthington",
                        "Oscar scored higher than the player who threw the orange darts",
                        "The contestant from Mount union threw the black darts",
                        "Pedro didn't finish with 55 points",
                        "The player who threw the red darts was either Colin or the contestant who scored 48 points",
                        "Of the contestant who scored 41 points and the person who threw the orange darts, one was from Gillbertville and the other was from Worthington",
                        "Ira scored 7 points lower than the player from Lohrville"
        ], [
                        noun(contestant, [contestant], [contestants]),
                        noun(contestant, [person], [persons]),
                        noun(contestant, [player], [players]),
                        pn(contestant, [bill]),
                        pn(contestant, [colin]),
                        pn(contestant, [ira]),
                        pn(contestant, [oscar]),
                        pn(contestant, [pedro]),
                        pn(darts, [the, black, darts]),
                        pn(darts, [the, orange, darts]),
                        pn(darts, [the, red, darts]),
                        pn(darts, [the, white, darts]),
                        pn(darts, [the, yellow, darts]),
                        pn(city, [mount, union]),
                        pn(city, [gillbertville]),
                        pn(city, [lohrville]),
                        pn(city, [worthington]),
                        pn(city, [yorktown]),
                        noun(score, [point], [points]),
                        tv(pred(contestant, darts), [threw], [throw]),
                        tv(pred(contestant, score), [scored], [score]),
                        ivpp(pred(contestant, score), [finishes], [with], [finish]),
                        prep(fun(city, contestant), [from])
        ])).


% ADAPTED!!!
% added "shirts" to clue 3
problem(p3, problem(4, 5, [
                        "The team that finished with 739 points wore orange shirts",
                        "Of the squad in the blue shirts and the team from Stacyville, one had a final score of 715 points and the other was the Oddballs",
                        "Of the Turkey Rolls and the team from Lathrop, one wore orange shirts and the other wore red shirts",
                        "The rowdy rollers finished 12 points below the team in orange shirts",
                        "The squad that finished with 727 points was either the team in the orange shirts or the squad from Woodstock",
                        "The Turkey Rolls weren't from Castro Valley",
                        "The Alley Cats had a final score of 763 points",
                        "The team in the blue shirts, the Splitters, the squad from Lathrop and the Rowdy Rollers were all different teams",
                        "The squad that finished with 715 points didn't wear lime green shirts"
                    ], [
                        noun(team, [team], [teams]),
                        ivpp(pred(team, score), [finished], [with], [finish]),
                        noun(point, [point], [points]),
                        tv(pred(team, color), [wore], [wear]),
                        pn(color, [orange, shirts]),
                        noun(team, [squad], [squads]),
                        prep(pred(color, team), [in]),
                        pn(color, [blue, shirts]),
                        prep(pred(city, team), [from]),
                        pn(city, [stacyville]),
                        ivpp(pred(team, score), [had, a, final, score], [of], [have, a, final, score, of]),
                        pn(team, [oddballs]),
                        pn(team, [turkey, rolls]),
                        pn(city, [lathrop]),
                        pn(color, [red, shirts]),
                        pn(team, [rowdy, rollers]),
                        pn(city, [woodstock]),
                        pn(city, [castro, valley]),
                        pn(team, [alley, cats]),
                        pn(team, [splitters]),
                        pn(color, [lime, green, shirts])
                    ])).

%ADAPTED
% dropped now from clue 7
% adapted clue 8 to be a real enumeration
% dropped "currently" from clue 10
problem(p4, problem(4, 5, [
                        "Michael isn't from Eldon",
                        "The 2008 graduate is either Jackie or Michael",
                        "The 2006 graduate is either Ivan or Orlando",
                        "The Dodgers player didn't graduate in 2007 or 2008",
                        "Jackie graduated 2 years before the player from York",
                        "The 2007 graduate isn't playing for the Giants",
                        "Of Lonnie and the person from Frenchboro, one is with the Dodgers and the other graduated in 2005",
                        %% "The five players are the person from Frenchboro, Ivan, and the three players currently with the Indians, Mariners and Giants",
                        "The five players are the person from Frenchboro, Ivan, the player with the Indians, the person with the Mariners and the graduate that plays for the Giants",
                        "The person who graduated in 2005 is from Unity",
                        "Lonnie plays for the Mariners"
                     ], [
                        pn(person, [michael]),
                        prep(pred(person, city), [from]),
                        pn(city, [eldon]),
                        noun(person, [graduate], [graduates]),
                        pn(person, [jackie]),
                        pn(person, [ivan]),
                        pn(person, [orlando]),
                        pn(team, [dodgers]),
                        noun(person, [player], [players]),
                        ivpp(pred(person, year), [graduated], [in], [graduate]),
                        noun(year, [year], [years]),
                        pn(city, [york]),
                        ivpp(pred(person, team), [plays], [for], [play]),
                        pn(team, [giants]),
                        pn(person, [lonnie]),
                        noun(person, [person], [persons]),
                        pn(city, [frenchboro]),
                        prep(pred(person, team), [with]),
                        pn(team, [indians]),
                        pn(team, [mariners]),
                        pn(city, [unity])
                     ])).


% ADAPTED!!!
% replaced "$... per share" with "... dollar per share"
problem(p11, problem(4, 5, [
                         "The utilities stock was 1 dollar per share less expensive than the stock Geraldo bought",
                         "GXTV was 2 dollar per share less expensive than QMZ",
                         "The financial stock wasn't purchased by Edith",
                         "PSTO sold for 29 dollar per share",
                         "The stock Abigail bought was either KMPP or JMO",
                         "The health-care stock was 2 dollar per share more expensive than the financial stock",
                         "The energy stock was less expensive than JMO",
                         "Heathcliff bought the real estate stock",
                         "Of QMZ and GXTV, one sold for 26 dollar per share and the other was in the health-care sector",
                         "Abigail didn't purchase the stock that sold for 25 dollar per share"
                     ], [
                         pn(sector, [utilities, stock]),
                         noun(price, [dollar, per, share], [dollars, per, share]),
                         % less expensive
                         noun(stock, [stock], [stocks]),
                         pn(person, [geraldo]),
                         tv(pred(person, stock), [bought], [buy]),
                         pn(stock, [gxtv]),
                         pn(stock, [qmz]),
                         pn(sector, [financial, stock]),
                         % does this work? (passive form)
                         ivpp(pred(stock, person), [purchased], [by], [purchase]),
                         pn(stock, [psto]),
                         ivpp(pred(stock, price), [sold], [for], [sell]),
                         pn(person, [abigail]),
                         pn(stock, [kmpp]),
                         pn(stock, [jmo]),
                         pn(sector, ['health-care', stock]),
                         % more expensive
                         pn(sector, [energy, stock]),
                         pn(person, [heathcliff]),
                         pn(sector, [real, estate, stock])
                         % in the health-care sector vs the health-care stock
                     ])).
