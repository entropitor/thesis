const fs = require('fs')

const input = fs.readFileSync('/dev/stdin').toString()
// console.log(input)

const NB_MODELS_REGEX = /Number of models: ([0-9]+)/

const nbModels = input.match(NB_MODELS_REGEX)[1]

const models = []
for (let i = 1; i <= nbModels; i++) {
  models.push(parseModel(i))
}

function parseModel (i) {
  const MODEL_REGEX = new RegExp(`Model ${i}\n=*\nstructure  : V {\n((.*\n)*?)}\n`)
  const predicates = input.match(MODEL_REGEX)[1].split('\n').slice(0, -1).map(x => parseLine(x.trim()))
  let groups = []

  for (let predicate of predicates) {
    for (let couple of predicate) {
      groups = addToGroups(couple, groups)
    }
  }

  return {
    i,
    groups
  }
}

function parseLine (line) {
  const LINE_REGEX = /.* = \{ (.*) \}/
  return line.match(LINE_REGEX)[1].split('; ').map(x => x.split(','))
}

function addToGroups ([a, b], groups) {
  let ia = groups.findIndex(g => g.indexOf(a) !== -1)
  let ib = groups.findIndex(g => g.indexOf(b) !== -1)

  if (ia === -1 && ib === -1) {
    return [
      ...groups,
      [a, b]
    ]
  } else if (ia !== -1 && ib === -1) {
    return [
      ...groups.filter((_, i) => i !== ia),
      [...groups[ia], b]
    ]
  } else if (ia === -1 && ib !== -1) {
    return [
      ...groups.filter((_, i) => i !== ib),
      [...groups[ib], a]
    ]
  } else if (ia !== -1 && ib !== -1) {
    return [
      ...groups.filter((_, i) => i !== ia && i !== ib),
      [...groups[ia], ...groups[ib]].filter(onlyUnique)
    ]
  }
}

function onlyUnique (value, index, self) {
  return self.indexOf(value) === index
}

function arrayToPrologArray (arr) {
  return `[${arr.join(',')}]`
}

function toPrologOutput (model) {
  return `model(${model.i}, ${arrayToPrologArray(model.groups.map(arrayToPrologArray))})`
}

console.log(`models(${nbModels}, ${arrayToPrologArray(models.map(toPrologOutput))}).`)
