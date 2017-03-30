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

