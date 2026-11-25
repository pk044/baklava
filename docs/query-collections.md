---
sidebar_position: 7
title: Collection Parameters
---

# Collection Parameters

A query or path parameter that carries several values can be written into a URL in more than one way. `?ids=1&ids=2&ids=3` and `?ids=1,2,3` are both "a list of three ids", and a server written for one usually rejects the other.

Baklava lets you declare which form an endpoint uses. That one declaration drives three things, so they cannot drift apart:

- the URL Baklava sends in the test,
- the `style` and `explode` fields in the generated OpenAPI document,
- the URL built by the generated ts-fetch and sttp clients, and the Postman collection.

## Query parameters

Pass `collectionStyle` to `q`:

```scala
import pl.iterators.baklava.CollectionStyle

supports(
  GET,
  queryParameters = (
    q[Seq[String]]("label"),
    q[Seq[Long]]("assigneeIds", collectionStyle = CollectionStyle.CommaSeparated),
    q[Option[Seq[String]]]("fields", collectionStyle = CollectionStyle.PipeDelimited)
  )
)(
  onRequest(queryParameters = (Seq("backend", "urgent"), Seq(7L, 9L), Some(Seq("id", "title"))))
    .respondsWith[Seq[Task]](OK)
    .assert { ctx => ctx.performRequest(allRoutes) }
)
```

The request goes out as `?label=backend&label=urgent&assigneeIds=7,9&fields=id%7Ctitle`.

| `CollectionStyle`     | OpenAPI `style`  | OpenAPI `explode` | URL for `ids = Seq(1, 2, 3)` |
| --------------------- | ---------------- | ----------------- | ---------------------------- |
| `Exploded` (default)  | `form`           | `true`            | `?ids=1&ids=2&ids=3`         |
| `CommaSeparated`      | `form`           | `false`           | `?ids=1,2,3`                 |
| `PipeDelimited`       | `pipeDelimited`  | `false`           | `?ids=1%7C2%7C3`             |
| `SpaceDelimited`      | `spaceDelimited` | `false`           | `?ids=1%202%203`             |

The style applies to `Seq`, `List`, `Vector`, `Set` and `Array`, and to an `Option` of any of them. A parameter that is not a collection always uses `form` with `explode: true`; giving it another style throws an `IllegalArgumentException` when the parameter is defined, because OpenAPI has no meaning for it.

An empty collection, or an absent optional one, leaves the parameter out of the URL for every style.

### Choosing a style

`Exploded` is the default because it is what server frameworks decode without extra work. The delimited styles arrive as a single string, and the route has to split it.

| Framework  | `Exploded`                             | `CommaSeparated`                                 |
| ---------- | -------------------------------------- | ------------------------------------------------ |
| Pekko HTTP | `parameters("ids".as[Int].*)`          | `parameter("ids".as(CsvSeq[Int]))`               |
| http4s     | `request.multiParams("ids")` has one entry per value | `request.multiParams("ids")` has one entry, `"1,2,3"`, to split |

Pekko HTTP has no built-in decoder for `PipeDelimited` or `SpaceDelimited`; a route receives `"1|2|3"` or `"1 2 3"` and splits it itself.

Pick a delimited style only when the server already expects it. If the route decodes repeated parameters and the test declares `CommaSeparated`, the test fails, which is the point: the documentation never claims a format the server does not accept.

### Values that contain the delimiter

Each value is percent-encoded on its own and the delimiter is written between them, so `Seq("a,b", "c")` with `CommaSeparated` becomes `?tags=a%2Cb,c`. A server that splits before decoding can tell the two commas apart; a server that decodes first cannot.

`PipeDelimited` and `SpaceDelimited` offer no such escape. A pipe inside a value is `%7C`, the same as the delimiter, so `Seq("a|b", "c")` reaches the server as three values, and the generated example shows three values. Use `Exploded` for free text.

## Path parameters

Pass `style` to `p`. Unlike the query styles, a path style also changes how a single value is written.

```scala
import pl.iterators.baklava.PathStyle

path("/reports/{ids}")(
  supports(GET, pathParameters = p[Seq[Int]]("ids", style = PathStyle.Matrix))(
    onRequest(pathParameters = Seq(3, 4, 5))
      .respondsWith[Report](OK)
      .assert { ctx => ctx.performRequest(allRoutes) }
  )
)
```

| `PathStyle`        | OpenAPI `style` | OpenAPI `explode` | Segment for `id = 5` | Segment for `id = Seq(3, 4, 5)` |
| ------------------ | --------------- | ----------------- | -------------------- | ------------------------------- |
| `Simple` (default) | `simple`        | `false`           | `5`                  | `3,4,5`                         |
| `Label`            | `label`         | `false`           | `.5`                 | `.3,4,5`                        |
| `LabelExploded`    | `label`         | `true`            | `.5`                 | `.3.4.5`                        |
| `Matrix`           | `matrix`        | `false`           | `;id=5`              | `;id=3,4,5`                     |
| `MatrixExploded`   | `matrix`        | `true`            | `;id=5`              | `;id=3;id=4;id=5`               |

Collections in a path are supported for the same types as in a query. Pekko HTTP and http4s hand the route the whole segment, prefix included (`.3,4,5`, `;id=3,4,5`), and the route parses it.

A custom `ToPathParam` that only defines `apply` is treated as a single value. Override `values` to make a custom type behave as a collection.

## What the generators emit

**OpenAPI** writes `style` and `explode` on every query and path parameter, including the defaults, because client generators disagree about what a missing pair means. The example of a collection parameter is an array of the values the test sent, typed like the items:

```yaml
- name: assigneeIds
  in: query
  required: true
  style: form
  explode: false
  schema:
    type: array
    items:
      type: integer
      format: int64
  example:
  - 7
  - 9
```

**ts-fetch** and **sttp-client** build the URL the same way the test did. The ts-fetch client goes through `URLSearchParams`, which percent-encodes the comma (`assigneeIds=7%2C9`); a server decodes that to the same value.

**Postman** repeats the key for `Exploded` and writes one joined value for the delimited styles. Path variables hold the full segment, prefix included.

**Simple HTML** shows the real request URL in its curl example.

**ts-rest** and **oRPC** contracts describe a collection as `z.array(...)` and nothing more. How it is written into the URL is decided by those libraries' own clients, not by `collectionStyle`. The same holds for path styles other than `Simple`.

## Adapters

Pekko HTTP and http4s deliver the URL to the route exactly as built.

The sttp adapter sends the query string exactly as built. The path still goes through sttp's URL normalisation, which un-escapes a comma inside a path value, so a path collection whose values contain commas is ambiguous on the wire when sent through sttp.

## Calls captured before this feature

Calls kept in the persistent store from an earlier Baklava version carry no style. They are read as `Exploded` and `Simple`, and their collection examples stay a single joined string until the suite that produced them runs again.

## Changes to be aware of when upgrading

- With five or more query parameters on one endpoint, the parameters are now written in declaration order. Earlier versions wrote them in an arbitrary order.
- A request whose declared query parameters are all absent or empty no longer ends in a dangling `?`.
- OpenAPI output gains `style` on query parameters and `style` plus `explode` on path parameters.
- A collection parameter's OpenAPI example is now an array instead of a comma-joined string.
- The generated ts-fetch client no longer comma-joins an exploded collection, and the generated sttp client no longer sends `List(1, 2, 3)`.
- The sttp adapter no longer rewrites percent-encoding in the query string, so reserved characters in a value stay percent-encoded (`a%3Ab`, not `a:b`).
