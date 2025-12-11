import * as A from "fp-ts/lib/Array.js";
import * as B from "fp-ts/lib/boolean.js";
import * as E from "fp-ts/lib/Either.js";
import { flow, identity, pipe } from "fp-ts/lib/function.js";
import { concatAll } from "fp-ts/lib/Monoid.js";
import * as NEA from "fp-ts/lib/NonEmptyArray.js";
import * as N from "fp-ts/lib/number.js";
import * as O from "fp-ts/lib/Option.js";
import * as Ord from "fp-ts/lib/Ord.js";
import * as R from "fp-ts/lib/Record.js";
import * as S from "fp-ts/lib/string.js";
import * as T from "fp-ts/lib/Tuple.js";
import * as fs from "fs/promises";
import * as path from "path";
import { solve } from "yalps";
import * as U from "./utils.js";
const dayNumber = 10;
// --- Data Preparation Helper ---
async function readAndParseData(filePath) {
    try {
        const data = await fs.readFile(path.resolve(filePath), "utf8");
        const lines = data.split("\n").filter((line) => line.trim() !== ""); // Read and filter empty lines
        // UPDATE FROM HERE
        return pipe(lines, A.map((line) => {
            const splLine = line.trim().split(" ");
            const lightsStr = A.takeLeft(1)(splLine);
            const buttonsStr = pipe(splLine, A.dropLeft(1), A.dropRight(1));
            const joltageStr = A.takeRight(1)(splLine);
            const lights = pipe(lightsStr[0].slice(1, -1).split(""), A.map((c) => c === "#"));
            const buttons = pipe(buttonsStr, A.map((tup) => pipe(tup.slice(1, -1).split(","), A.map(parseInt))));
            const joltages = pipe(joltageStr[0].slice(1, -1).split(","), A.map(parseInt));
            return { lights, buttons, joltages };
        }));
        // UPDATE UNTIL HERE
    }
    catch (error) {
        console.error("Error reading file:", error);
        throw error;
    }
}
// --- Part A Logic ---
const pressButtonA = (lights) => (button) => pipe(button, A.reduce(lights, (ls, l) => pipe(ls, A.mapWithIndex((i, x) => (i === l ? !x : x)))));
const stepA = ({ lights, buttons, depth }) => pipe(buttons, A.mapWithIndex((i, b) => pipe(b, pressButtonA(lights), (newLights) => ({
    lights: newLights,
    buttons: pipe(buttons, A.deleteAt(i), O.match(() => [], identity)),
    depth: depth + 1,
}))));
const BFSA = (goal) => (states) => pipe(states, A.map((state) => A.getEq(B.Eq).equals(goal, state.lights)), concatAll(B.MonoidAny), (foundGoal) => foundGoal || states[0].depth > 10
    ? states[0].depth
    : pipe(states, A.flatMap(stepA), BFSA(goal)));
const partA = (parsed) => pipe(parsed, A.map((entry) => BFSA(entry.lights)([
    {
        lights: A.replicate(entry.lights.length, false),
        buttons: entry.buttons,
        depth: 0,
    },
])), concatAll(N.MonoidSum));
// --- Part B Logic ---
const mkModel = (entry) => {
    const constraints = pipe(entry.joltages, A.mapWithIndex((i, jolt) => [
        `j${i}`,
        { min: jolt, max: jolt },
    ]), R.fromEntries);
    const variables = pipe(entry.buttons, A.mapWithIndex((i, button) => [
        `b${i}`,
        pipe(button, A.map((b) => [`j${b}`, 1]), A.concat([["obj", 1]]), R.fromEntries),
    ]), R.fromEntries);
    return {
        direction: "minimize",
        objective: "obj",
        constraints,
        variables,
        integers: pipe(entry.buttons, A.mapWithIndex((i, b) => `b${i}`)),
    };
};
const partB = (parsed) => pipe(parsed, A.map(flow(mkModel, solve, (x) => x.result)), concatAll(N.MonoidSum));
// --- Execution ---
async function main() {
    const file = `inputs/day${dayNumber}input.txt`;
    const startParse = performance.now();
    const parsed = await readAndParseData(file);
    const startA = performance.now();
    const resultA = partA(parsed);
    const startB = performance.now();
    const resultB = partB(parsed);
    const end = performance.now();
    console.log(resultA);
    console.log(resultB);
    console.log(`Parsing took ${(startA - startParse).toFixed(3)}ms`);
    console.log(`Part A took ${(startB - startA).toFixed(3)}ms`);
    console.log(`Part B took ${(end - startB).toFixed(3)}ms`);
}
main(); // Uncomment to run
// --- Just for importing ---
const uncalled = () => {
    const a = pipe([], A.map(() => ["", 0]), R.fromEntries, R.toEntries, A.map(T.fst), B.isBoolean);
    const b = flow((x) => U.modulo(x, 5));
    const c = pipe(NEA.range(1, 5), concatAll(N.MonoidSum));
    const d = pipe([1, 2, 3, -1, 5, -7], A.map(flow(JSON.stringify, O.some)), A.reduce(O.none, Ord.max(O.getOrd(S.Ord))));
    const e = N.Eq;
};
//# sourceMappingURL=day10.js.map