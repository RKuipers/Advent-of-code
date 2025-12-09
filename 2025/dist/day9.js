import * as A from "fp-ts/lib/Array.js";
import * as B from "fp-ts/lib/boolean.js";
import * as E from "fp-ts/lib/Either.js";
import { flow, identity, pipe } from "fp-ts/lib/function.js";
import * as M from "fp-ts/lib/Monoid.js";
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
const dayNumber = 9;
// --- Data Preparation Helper ---
async function readAndParseData(filePath) {
    try {
        const data = await fs.readFile(path.resolve(filePath), "utf8");
        const lines = data.split("\n").filter((line) => line.trim() !== ""); // Read and filter empty lines
        // UPDATE FROM HERE
        const result = pipe(lines, A.map((line) => pipe(line.trim().split(","), A.map(parseInt), (l) => l.length === 2 ? [l[0], l[1]] : [0, 0])));
        return result;
        // UPDATE UNTIL HERE
    }
    catch (error) {
        console.error("Error reading file:", error);
        throw error;
    }
}
// --- Part A Logic ---
const rect = (a) => (b) => (Math.abs(a[0] - b[0]) + 1) * (Math.abs(a[1] - b[1]) + 1);
const partA = (parsed) => pipe(parsed, A.mapWithIndex((i, coord) => pipe(parsed, A.dropLeft(i + 1), A.map(rect(coord)))), A.flatten, A.foldMap(M.max(N.Bounded))(identity));
const resultOrd = Ord.contramap((x) => x.area)(N.Ord);
// TODO:
// Same as part a, but filter ones where the rect doesn't intersect poly
// Start with the 2 big lines to check (A.rotate)
const isHorizontal = (line) => line[0][1] === line[1][1];
const linesIntersect = ([h1, h2]) => ([v1, v2]) => v1[0] > Ord.min(N.Ord)(h1[0], h2[0]) &&
    v1[0] < Ord.max(N.Ord)(h1[0], h2[0]) &&
    h1[1] > Ord.min(N.Ord)(v1[1], v2[1]) &&
    h1[1] < Ord.max(N.Ord)(v1[1], v2[1]);
const edgesIntersect = (polyH, polyV) => (p1, p2) => {
    const h1 = [
        [p1[0], p1[1]],
        [p2[0], p1[1]],
    ];
    const h2 = [
        [p1[0], p2[1]],
        [p2[0], p2[1]],
    ];
    const v1 = [
        [p1[0], p1[1]],
        [p1[0], p2[1]],
    ];
    const v2 = [
        [p2[0], p1[1]],
        [p2[0], p2[1]],
    ];
    for (const l of polyH) {
        if (linesIntersect(l)(v1) || linesIntersect(l)(v2))
            return true;
    }
    for (const l of polyV) {
        if (linesIntersect(h1)(l) || linesIntersect(h2)(l))
            return true;
    }
    return false;
};
const inRectangle = (coord1, coord2) => (p) => p[0] > Ord.min(N.Ord)(coord1[0], coord2[0]) &&
    p[0] < Ord.max(N.Ord)(coord1[0], coord2[0]) &&
    p[1] > Ord.min(N.Ord)(coord1[1], coord2[1]) &&
    p[1] < Ord.max(N.Ord)(coord1[1], coord2[1]);
const pointsInRectangle = (coord1, coord2) => (ps) => {
    for (const p of ps)
        if (inRectangle(coord1, coord2)(p))
            return true;
    return false;
};
const partB = (parsed) => pipe(parsed, A.mapWithIndex((i, p) => [
    parsed[i + 1] ??
        pipe(parsed, A.head, O.match(() => p, identity)),
    p,
]), A.sort(Ord.contramap(([p1, p2]) => Math.abs(p1[0] - p2[0]) + Math.abs(p1[1] - p2[1]))(N.Ord)), A.reverse, A.partition(isHorizontal), (x) => x, (poly) => pipe(parsed, A.mapWithIndex((i, coord1) => pipe(parsed, A.dropLeft(i + 1), A.filter((coord2) => {
    const polyInsideRect = pointsInRectangle(coord1, coord2)(parsed);
    const edgesInter = edgesIntersect(poly.right, poly.left)(coord1, coord2);
    return !(polyInsideRect || edgesInter);
}), A.map((coord2) => ({
    coord1,
    coord2,
    area: rect(coord1)(coord2),
})))), (x) => x), (x) => x, A.flatten, (x) => x, A.sort(resultOrd), A.reverse, A.takeLeft(5), A.map((x) => {
    const polyList = `(${x.coord1[0]},${x.coord1[1]}),` +
        `(${x.coord2[0]},${x.coord1[1]}),` +
        `(${x.coord2[0]},${x.coord2[1]}),` +
        `(${x.coord1[0]},${x.coord2[1]})`;
    console.log(`polygon([${polyList}])`);
    console.log(`area: ${x.area}`);
    return x.area;
}), A.head, (x) => (O.isSome(x) ? x.value : 0));
const partBTried = (parsed) => pipe(parsed, A.filterMap(([x, y]) => y >= 50233 && y <= 69349
    ? O.some({
        coord1: [x, y],
        coord2: [94693, 50233],
        area: rect([x, y])([94693, 50233]),
    })
    : y <= 48547 && y >= 32338
        ? O.some({
            coord1: [x, y],
            coord2: [94693, 48547],
            area: rect([x, y])([94693, 48547]),
        })
        : O.none), A.sort(resultOrd), A.reverse, A.takeLeft(5), A.map((x) => {
    const polyList = `(${x.coord1[0]},${x.coord1[1]}),` +
        `(${x.coord2[0]},${x.coord1[1]}),` +
        `(${x.coord2[0]},${x.coord2[1]}),` +
        `(${x.coord1[0]},${x.coord2[1]})`;
    console.log(`polygon([${polyList}])`);
    console.log(`area: ${x.area}`);
    return x.area;
}), (x) => 0);
const partBWrite = (polygon) => pipe(polygon, A.map((p) => `(${p})`), (x) => {
    console.log(`polygon([${x.join(",")}])`);
    return 0;
});
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
//# sourceMappingURL=day9.js.map