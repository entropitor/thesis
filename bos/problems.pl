:- module(problems, [problem/2]).

problem(vakantiedagen, [
            "If the age of an employee is less than 18 or greather than 60 then he receives 5 extra days",
            "If the years of service of an employee is at least 30, then he receives 5 extra days",
            "If years of service of an employee is at least 30 then he receives 3 extra days",
            "If age of an employee is at least 60 then he receives 3 extra days",
            "If the years of service of an employee is between 15 and 30 and he does not receive 5 extra days then he receives 2 extra days",
            "If the age of an employee is at least 45 and he does not receive 5 extra days then he receives 2 extra days",
            "Each employee has a number of vacation days equal to 22 plus the sum of the extra days he receives"
        ]).

% ADAPTED!!!
problem(p1, [
            "The perens pig lives in Slovakia and is recognized as endangered in 2009",
            "The eldar elk has a population size of 210",
            "The byengo bat doesn't live in Ghana",
            "The animal that lives in Slovakia is recognized as endangered before the animal from Russia",
            "Of the species that has a population size of 490 and the eldar elk, one lives in Poland and the other is recognized as endangered in 2009",
            "Neither the nibner newt nor the byengo bat has a population size of 525",
            "The species that lives in Ghana is recognized as endangered 2 years after the nibner newt",
            "The animal that lives in Russia doesn't have a population size of 315"
        ]).

% ADAPTED!!!
problem(p2, [
            "Of the contestant who scored 41 points and the person who threw the white darts, one was from Worthington and the other was Ira",
            "Bill is from Mount union",
            "Ira scored 21 points higher than the contestant who threw the orange darts",
            "The contestant from Mount union threw the black darts",
            "Pedro didn't score 55 points",
            "The contestant who threw the red darts was either Colin or the contestant who scored 48 points",
            "Of the contestant who scored 41 points and the contestant who threw the orange darts, one was from Gillbertville and the other was from Worthington",
            "Ira scored 7 points lower than the contestant from Lohrville"
        ]).

problem(p2counts, [
            "Ira threw the black darts",
            "Ira scored 5 points",
            "Ira scored 5 points lower than 41 points",
            "Ira scored 5 points lower than the total that Pedro scored",
            "Ira scored 5 points lower than Pedro",
            "Ira scored 5 points lower than the person who threw the orange darts",
            "Ira scored lower than the person who threw the orange darts"
        ]).

problem(alldifferent, [
            "Ira, colin and bill are all different contestants",
            "Ira, colin, bill and the contestant who threw the orange darts are 4 different contestants",
            "The five contestants are the contestant who threw the orange darts; Ira; and the three contestants from Mount union, yorktown and lohrville"
            %% "The contestant who scored 41 points, the contestant who threw the orange darts and Ira are three different contestants"
        ]).

problem(zebra, [
            "The Englishman lives in the red house",
            "The Spaniard keeps the dog",
            "There is a person who lives in the green house and drinks coffee",
            "The Ukrainian drinks tea",
            "The position of the green house is equal to the position of the ivory house plus 1",
            "There is a person who smokes Old Gold and keeps the snail",
            "There is a person who lives in a yellow house and smokes Kools",
            "The person who lives in the third house, drinks milk",
            "The Norwegian lives in the first house",
            "A house A is next to a house B if the absolute value of the difference between the position of house A and the position of house B is equal to 1",
            "The house in which the person who smokes Chesterfields lives, is next to the house in which the person who keeps the fox lives",
            "The house in which the person who smokes Kools lives, is next to the house in which the person who keeps the horse lives",
            % "The person who smokes Lucky Strike, drinks Orange Juice",
            "The smoker of Lucky Strike, drinks Orange Juice",
            "The Japanese smokes Parliaments",
            "The Norwegian lives in a house that is next to the blue house",
            "Every animal is kept by exactly 1 Person",
            "Who drinks water",
            "Who keeps the zebra"
      ]).

problem(extra, [
            "There is a blue house or every person who smokes Lucky Strike, drinks Orange Juice",
            "There is a blue house in which the Norwegian lives or every person who smokes Lucky Strike drinks Orange Juice",
            "Every blue house is next to a red house or is next to every green house",
            "Every blue house is next to every green house or is next to a red house",
            %"Every blue house is next to a red house in which the person who drinks tea lives or is next to the green house",
            "Every blue house is next to the green house or is next to a red house in which the person who drinks tea lives",
            "There is a person who lives in the green house",
            "Every person who drinks tea lives in a blue house",
            "There is a person who lives in the green house and drinks coffee and keeps the snail",
            "There is a person who lives in the green house and lives in a red house and lives in every blue house",
            "There is a house in which the Norwegian lives and the Ukrainian lives and the Spaniard lives",
            "The person who lives in the green house or lives in the red house, lives in the blue house"
        ]).
