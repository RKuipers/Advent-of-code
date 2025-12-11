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
import * as U from "./utils.js";
const dayNumber = 11;
// --- Data Preparation Helper ---
async function readAndParseData(filePath) {
    try {
        const data = await fs.readFile(path.resolve(filePath), "utf8");
        const lines = data.split("\n").filter((line) => line.trim() !== ""); // Read and filter empty lines
        // UPDATE FROM HERE
        return pipe(lines, A.map((line) => {
            const splLine = line.trim().split(": ");
            if (splLine.length !== 2)
                throw "parsing error";
            return [splLine[0], E.right(splLine[1].split(" "))];
        }), R.fromEntries);
        // UPDATE UNTIL HERE
    }
    catch (error) {
        console.error("Error reading file:", error);
        throw error;
    }
}
// --- Part A Logic ---
const updateRec = (current, val) => (conns) => pipe(conns, R.updateAt(current, E.left(val)), O.match(() => conns, identity));
const DFS = (current, goal) => (connections) => current === goal
    ? {
        conns: updateRec(current, 1)(connections),
        paths: 1,
    }
    : pipe(connections, R.lookup(current), O.match(() => ({ conns: connections, paths: 0 }), (res) => E.isLeft(res)
        ? { conns: connections, paths: res.left }
        : pipe(res.right, A.reduce({ conns: connections, paths: 0 }, (acc, neigh) => pipe(acc.conns, DFS(neigh, goal), (res) => ({
            conns: res.conns,
            paths: res.paths + acc.paths,
        }))), (result) => ({
            conns: updateRec(current, result.paths)(result.conns),
            paths: result.paths,
        }))));
const partA = flow(DFS("you", "out"), (result) => result.paths);
// --- Part B Logic ---
const partB = (parsed) => DFS("svr", "fft")(parsed).paths *
    DFS("fft", "dac")(parsed).paths *
    DFS("dac", "out")(parsed).paths +
    DFS("svr", "dac")(parsed).paths *
        DFS("dac", "fft")(parsed).paths *
        DFS("fft", "out")(parsed).paths;
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
//# sourceMappingURL=day11.js.map