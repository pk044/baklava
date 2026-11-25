package baklavaclient.tasks

import sttp.client4._
import sttp.client4.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import sttp.model.Uri

object TasksEndpoints {

  /** List tasks — List all tasks in a project */
  def listTasks(
      baseUri: Uri,
      oauth2Token: String
  )(
      projectId: Long,
      label: Seq[String],
      assigneeIds: Seq[Long],
      fields: Option[Seq[String]] = None
  ): Request[Either[ResponseException[String], Seq[Task]]] = {
    basicRequest
      .get(baseUri.addPath("projects", s"$projectId", "tasks")        .addParams(label.map(value => "label" -> value.toString): _*)
        .addParam("assigneeIds", if (assigneeIds.isEmpty) None else Some(assigneeIds.map(_.toString).mkString(",")))
        .addParam("fields", fields.filter(_.nonEmpty).map(_.map(_.toString).mkString("|"))))
      .header("Authorization", s"Bearer ${oauth2Token}")
      .response(asJson[Seq[Task]])
  }

  /** Create task — Create a task in a project */
  def createTask(
      baseUri: Uri,
      oauth2Token: String
  )(
      projectId: Long,
      body: CreateTaskRequest
  ): Request[Either[ResponseException[String], Task]] = {
    basicRequest
      .post(baseUri.addPath("projects", s"$projectId", "tasks"))
      .header("Authorization", s"Bearer ${oauth2Token}")
      .body(body.asJson.noSpaces)
      .contentType("application/json")
      .response(asJson[Task])
  }
}
