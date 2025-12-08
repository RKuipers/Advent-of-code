import * as A from "fp-ts/lib/Array.js";
import * as B from "fp-ts/lib/boolean.js";
import * as E from "fp-ts/lib/Either.js";
import type { Eq } from "fp-ts/lib/Eq.js";
import { flow, pipe } from "fp-ts/lib/function.js";
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

// npx tsc && node dist/day1.js

type Coord = [number, number, number];
type ParseType = Array<Coord>;
type PartType = (parsed: ParseType) => number;
const dayNumber = 8;
// --- Data Preparation Helper ---

async function readAndParseData(filePath: string): Promise<ParseType> {
  try {
    const data = await fs.readFile(path.resolve(filePath), "utf8");
    const lines = data.split("\n").filter((line) => line.trim() !== ""); // Read and filter empty lines

    // UPDATE FROM HERE

    const result = pipe(
      lines,
      A.map<string, Coord>((line) =>
        pipe(line.trim().split(","), A.map(parseInt), (coord) => [
          coord[0]!,
          coord[1]!,
          coord[2]!,
        ])
      )
    );

    return result;

    // UPDATE UNTIL HERE
  } catch (error) {
    console.error("Error reading file:", error);
    throw error;
  }
}

// --- Part A Logic ---

const dist = ([a, b]: [Coord, Coord]): number =>
  Math.sqrt((a[0] - b[0]) ** 2 + (a[1] - b[1]) ** 2 + (a[2] - b[2]) ** 2);

const CoordEq: Eq<Coord> = {
  equals: (a, b) => a[0] === b[0] && a[1] === b[1] && a[2] === b[2],
};

const findClusterIndex = (coord: Coord) => (clusters: Array<Array<Coord>>) =>
  pipe(
    clusters,
    A.findIndex((cl) => A.elem(CoordEq)(coord)(cl))
  );

const connect =
  (edge: [Coord, Coord]) =>
  (clusters: Array<Array<Coord>>): Array<Array<Coord>> => {
    const cluster1i = findClusterIndex(edge[0])(clusters);
    const cluster2i = findClusterIndex(edge[1])(clusters);

    if (O.isNone(cluster1i) || O.isNone(cluster2i)) return [];

    if (cluster1i.value === cluster2i.value) return clusters;

    const filteredClusters = pipe(
      clusters,
      A.deleteAt(Math.max(cluster1i.value, cluster2i.value)),
      O.flatMap(A.deleteAt(Math.min(cluster1i.value, cluster2i.value)))
    );
    const cluster1 = A.lookup(cluster1i.value)(clusters);
    const cluster2 = A.lookup(cluster2i.value)(clusters);

    const x =
      O.isSome(filteredClusters) && O.isSome(cluster1) && O.isSome(cluster2)
        ? [
            ...filteredClusters.value,
            pipe(cluster1.value, A.concat(cluster2.value), A.uniq(CoordEq)),
          ]
        : [];

    return x;
  };

const connectAll =
  (edges: Array<[Coord, Coord]>) =>
  (clusters: Array<Array<Coord>>): Array<Array<Coord>> =>
    edges.length === 0
      ? clusters
      : pipe(edges, A.splitAt(1), ([edge, edgeRest]) =>
          pipe(clusters, connect(edge[0]!), connectAll(edgeRest))
        );

const partA = (parsed: ParseType): number =>
  pipe(
    parsed,
    A.map((coord) => [coord]),
    (clusters) =>
      pipe(
        parsed,
        A.mapWithIndex((i, c1) =>
          pipe(
            parsed,
            A.dropLeft(i + 1),
            A.map((c2): [Coord, Coord] => [c1, c2])
          )
        ),
        A.flatten,
        A.map((t): [[Coord, Coord], number] => [t, dist(t)]),
        A.sort<[[Coord, Coord], number]>({
          compare: (a, b) => N.Ord.compare(a[1], b[1]),
          equals: (a, b) => N.Ord.equals(a[1], b[1]),
        }),
        A.takeLeft(1000),
        A.map((pair) => pair[0]),
        (edges) => connectAll(edges)(clusters),
        A.map(A.size),
        A.sort(N.Ord),
        A.takeRight(3),
        concatAll(N.MonoidProduct)
      )
  );

// --- Part B Logic ---

const connectUntilRecursive =
  (edges: Array<[Coord, Coord]>) =>
  (clusters: Array<Array<Coord>>): [Coord, Coord] =>
    clusters.length === 2
      ? pipe(
          edges,
          A.findFirst(
            (edge) =>
              !O.getEq(N.Eq).equals(
                findClusterIndex(edge[0])(clusters),
                findClusterIndex(edge[1])(clusters)
              )
          ),
          (edge) =>
            O.isSome(edge)
              ? edge.value
              : [
                  [0, 0, 0],
                  [0, 0, 0],
                ] // Error
        )
      : pipe(edges, A.splitAt(1), ([edge, edgeRest]) =>
          pipe(clusters, connect(edge[0]!), connectUntilRecursive(edgeRest))
        );

const connectUntilLoop =
  (edges: Array<[Coord, Coord]>) =>
  (clusters: Array<Array<Coord>>): [Coord, Coord] => {
    var clust = clusters;

    for (const edge of edges) {
      if (clust.length === 2)
        return pipe(
          edges,
          A.findFirst(
            (edge) =>
              !O.getEq(N.Eq).equals(
                findClusterIndex(edge[0])(clust),
                findClusterIndex(edge[1])(clust)
              )
          ),
          (edge) =>
            O.isSome(edge)
              ? edge.value
              : [
                  [0, 0, 0],
                  [0, 0, 0],
                ] // Error
        );

      clust = connect(edge)(clust);
    }

    return [
      [0, 0, 0],
      [0, 0, 0],
    ]; // eroor
  };

const partB = (parsed: ParseType): number =>
  pipe(
    parsed,
    A.map((coord) => [coord]),
    (clusters) =>
      pipe(
        parsed,
        A.mapWithIndex((i, c1) =>
          pipe(
            parsed,
            A.dropLeft(i + 1),
            A.map((c2): [Coord, Coord] => [c1, c2])
          )
        ),
        A.flatten,
        A.map((t): [[Coord, Coord], number] => [t, dist(t)]),
        A.sort<[[Coord, Coord], number]>({
          compare: (a, b) => N.Ord.compare(a[1], b[1]),
          equals: (a, b) => N.Ord.equals(a[1], b[1]),
        }),
        A.map((pair) => pair[0]),
        (edges) => connectUntilLoop(edges)(clusters),
        (edge) => edge[0][0] * edge[1][0]
      )
  );

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
  type opt<A> = O.Option<A>;
  type eth<E, A> = E.Either<E, A>;
  const a = pipe(
    [],
    A.map(() => ["", 0] as const satisfies [string, number]),
    R.fromEntries,
    R.toEntries,
    A.map(T.fst),
    B.isBoolean
  );
  const b = flow((x: number) => U.modulo(x, 5));
  const c = pipe(NEA.range(1, 5), concatAll(N.MonoidSum));
  const d = pipe(
    [1, 2, 3, -1, 5, -7],
    A.map(flow(JSON.stringify, O.some)),
    A.reduce(O.none, Ord.max(O.getOrd(S.Ord)))
  );
};
