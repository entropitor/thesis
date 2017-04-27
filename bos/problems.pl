:- module(problems, [problem/2]).

:- use_module(problemsPosterEvaluation, [problem/2 as problemPosterEvaluation]).

problem(X, Y) :-
    problemPosterEvaluation(X, Y).

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
                        ppn(darts, [the, black, darts]),
                        ppn(darts, [the, orange, darts]),
                        ppn(darts, [the, red, darts]),
                        ppn(darts, [the, white, darts]),
                        ppn(darts, [the, yellow, darts]),
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
                        ppn(team, [oddballs]),
                        ppn(team, [turkey, rolls]),
                        pn(city, [lathrop]),
                        pn(color, [red, shirts]),
                        ppn(team, [rowdy, rollers]),
                        pn(city, [woodstock]),
                        pn(city, [castro, valley]),
                        ppn(team, [alley, cats]),
                        ppn(team, [splitters]),
                        pn(color, [lime, green, shirts])
                    ])).

%ADAPTED
% isn't playing -> doesn't play (clue 6)
% dropped now from clue 7
% adapted clue 8 to be a real enumeration
% dropped "currently" from clue 10
problem(p4, problem(4, 5, [
                        "Michael isn't from Eldon",
                        "The 2008 graduate is either Jackie or Michael",
                        "The 2006 graduate is either Ivan or Orlando",
                        "The Dodgers player didn't graduate in 2007 or 2008",
                        "Jackie graduated 2 years before the player from York",
                        "The 2007 graduate doesn't play for the Giants",
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

problem(p5, problem(4, 5, [
                        "Mattie is 113 years old",
                        "The person who lives in Tehama is a native of either Kansas or Oregon",
                        "The Washington native is 1 year older than Ernesto",
                        "Roxanne is 2 years younger than the Kansas native",
                        "The person who lives in Zearing isn't a native of Alaska",
                        "The person who is 111 years old doesn't live in Plymouth",
                        "The Oregon native is either Zachary or the person who lives in Tehama",
                        "The person who lives in Shaver Lake is 1 year younger than Roxanne",
                        "The centenarian who lives in Plymouth isn't a native of Alaska",
                        "Of the person who lives in Tehama and Mattie, one is a native of Alaska and the other is from Kansas"
                     ], [
                        pn(person, [mattie]),
                        tvgap(pred(person, year), [is], [old], [are]),
                        noun(year, [year], [years]),
                        noun(person, [person], [persons]),
                        ivpp(pred(person, city), [lives], [in], [live]),
                        pn(city, [tehama]),
                        noun(person, [native], [natives]),
                        pn(state, [kansas]),
                        pn(state, [oregon]),
                        pn(state, [washington]),
                        comp(higher, [older, than]),
                        pn(person, [ernesto]),
                        pn(person, [roxanne]),
                        comp(lower, [younger, than]),
                        pn(city, [zearing]),
                        pn(state, [alaska]),
                        pn(city, [plymouth]),
                        pn(person, [zachary]),
                        pn(city, [shaver, lake]),
                        noun(person, [centenarian], [centenarians]),
                        prep(pred(person, state), [a, native, of]),
                        prep(pred(person, state), [from])
                     ])).

% ADAPTED
% clue 4: of the two dogs who graduated in March and April -> of the dog who graduated in March and the dog who graduated in April, ...
% clue 10: dropped sometime
problem(p6, problem(4, 5, [
                        "Officer Quinn's dog graduated in either March or April",
                        "Aries didn't graduate in March, May or June",
                        "The dog who graduated in March didn't go to Cole County",
                        "Of the dog who graduated in March and the dog who graduated in April, one went to Tanager County and the other was assigned to Officer Ingram",
                        "Aries wasn't assigned to Officer Ingram",
                        "Barca didn't go to Kermit County",
                        "The dog sent to Kermit County, the K-9 unit assigned to Officer Lyons and the canine who graduated in June are three different dogs",
                        "The dog assigned to Officer Salinas graduated 2 months after Aries",
                        "Of the dog sent to Sycamore County and Tinkerbell, one graduated in July and the other was assigned to Officer Underwood",
                        "Jaws graduated after the dog sent to Tanager County",
                        "McGruff went to Sycamore County"
        ], [
                        pn([officer, quinn]),
                        pn([aries]),
                        pn([barca]),
                        pn([cole, county]),
                        pn([tanager, county]),
                        pn([officer, ingram]),
                        pn([kermit, county]),
                        pn([officer, salinas]),
                        pn([sycamore, county]),
                        pn([tinkerbell]),
                        pn([officer, underwood]),
                        pn([officer, lyons]),
                        pn([jaws]),
                        pn([mcgruff]),
                        noun([dog], [dogs]),
                        noun([k9, unit], [k9, units]),
                        noun([canine], [canines]),
                        noun([month], [months]),
                        tvPrep([graduated], [in], [graduate], [graduated]),
                        tvPrep([went], [to], [go], [went]),
                        tvPrep([assigned], [to], [assign], [assigned]),
                        tvPrep([sent], [to], [send], [sent]),
                        pnn([march], 3),
                        pnn([april], 4),
                        pnn([may], 5),
                        pnn([june], 6),
                        pnn([july], 7)
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
