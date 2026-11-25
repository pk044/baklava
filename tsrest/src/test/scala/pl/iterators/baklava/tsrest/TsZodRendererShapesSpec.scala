package pl.iterators.baklava.tsrest

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import pl.iterators.baklava.*
import pl.iterators.baklava.tscommon.{TsSchemaRefs, TsZodDialect, TsZodRenderer}

object TsZodRendererShapesFixtures {
  sealed trait Payment
  final case class Card(last4: String, brand: String) extends Payment
  case object Cash                                    extends Payment

  final case class Category(name: String, children: List[Category], updatedAt: Option[String])

  sealed trait Expr
  final case class Lit(value: Int)              extends Expr
  final case class Add(left: Expr, right: Expr) extends Expr
}

class TsZodRendererShapesSpec extends AnyFunSpec with Matchers {
  import TsZodRendererShapesFixtures._

  private def hoisted(
      dialect: TsZodDialect,
      schemas: BaklavaSchemaSerializable*
  ): (Map[BaklavaSchemaSerializable, String], TsZodRenderer) = {
    val refs      = TsSchemaRefs.buildRefs(schemas, new TsZodRenderer(dialect).zodDefinition)
    val classRefs = TsSchemaRefs.classRefs(refs)
    (refs, new TsZodRenderer(dialect, refs.get, classRefs.get, classRefs.values.toSet))
  }

  private def fileFor(dialect: TsZodDialect, schemas: BaklavaSchemaSerializable*): String = {
    val (refs, renderer) = hoisted(dialect, schemas*)
    TsSchemaRefs.schemasFileContent(refs, renderer.zodDefinition, explicitType = renderer.explicitType)
  }

  describe("a discriminated sum") {
    val payment = BaklavaSchemaSerializable(Schema.derivedSum[Payment](SumEncoding.Discriminator("type")))

    it("renders z.discriminatedUnion, extending hoisted subtypes with the tag literal") {
      val (_, renderer) = hoisted(TsZodDialect.tsRest, payment)
      renderer.zodDefinition(payment) shouldBe
      """z.discriminatedUnion("type", [cardSchema.extend({"type": z.literal("Card")}), z.object({"type": z.literal("Cash")})])"""
    }

    it("hoists the sum itself under its class name") {
      fileFor(TsZodDialect.tsRest, payment) should include("export const paymentSchema = z.discriminatedUnion(")
    }
  }

  describe("the other encodings") {
    it("render wrapper objects and untagged alternatives as z.union") {
      val renderer = new TsZodRenderer(TsZodDialect.tsRest)
      val wrapped  = renderer.zodDefinition(BaklavaSchemaSerializable(Schema.derivedSum[Payment](SumEncoding.WrapperObject)))
      wrapped.replaceAll("\\s+", "") shouldBe
      """z.union([z.object({"Card":z.object({"brand":z.string(),"last4":z.string()})}),z.object({"Cash":z.object({})})])"""
      renderer.zodDefinition(BaklavaSchemaSerializable(implicitly[Schema[Either[String, Int]]])) shouldBe
      "z.union([z.string(), z.number().int()])"
    }
  }

  describe("a recursive case class") {
    val category = BaklavaSchemaSerializable(implicitly[Schema[Category]])

    it("references itself through z.lazy and gets an explicit type") {
      val file = fileFor(TsZodDialect.tsRest, category)
      file should include(
        """export type Category = { "children": Array<Category>; "name": string; "updatedAt"?: string | null | undefined };"""
      )
      file should include("export const categorySchema: z.ZodType<Category> = z.object(")
      file should include(""""children": z.array(z.lazy(() => categorySchema))""")
      file should not include "z.infer<typeof categorySchema>"
    }
  }

  describe("a recursive sum") {
    val expr = BaklavaSchemaSerializable(Schema.derivedSum[Expr](SumEncoding.Discriminator("op")))

    it("spells out cycle members in the explicit type instead of inferring them") {
      val file = fileFor(TsZodDialect.orpc, expr)
      file should include(
        """export type Expr = ({ "op": "Add" } & { "left": Expr; "right": Expr }) | ({ "op": "Lit" } & z.infer<typeof litSchema>);"""
      )
      file should include("export const exprSchema: z.ZodType<Expr> = z.discriminatedUnion(\"op\", [addSchema.extend(")
      file should include(""""left": z.lazy(() => exprSchema)""")
    }
  }
}
