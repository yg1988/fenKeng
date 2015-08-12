package roll
package gameplay

import acyclic.file
import org.scalajs.dom.ext.KeyCode
import async.Async._
import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js._
import scala.scalajs.js.Any._
import cp.Implicits._
import org.scalajs.dom
import org.scalajs.dom.svg._
import roll.cp
import org.scalajs.dom
import scala.concurrent.{ExecutionContext, Promise, Future}
import roll.gameplay.modules._


object Level {

  case class Input(keys: Set[Int],
                   keyPresses: Set[Int],
                   touches: Seq[Touch],
                   screenSize: cp.Vect,
                   painter: dom.CanvasRenderingContext2D)

  trait Result

  object Result {

    case object Next extends Result

    case object Reset extends Result

    case object Exit extends Result

  }

  var mouseDown = false

}

class Level(src: String, initialDims: cp.Vect) extends Level.Result {


  //dom.localStorage.setItem("tigerPlayers", "[{\"id\":0,\"isHuman\":true,\"intelligenceLevel\":\"2\",\"name\":\"P0\",\"color\":\"black\",\"state\":\"\"},{\"id\":1,\"isHuman\":false,\"intelligenceLevel\":\"2\",\"name\":\"P1\",\"color\":\"blue\",\"state\":\"\"}]");
  val playerInfos = JSON.parse(dom.localStorage.getItem("tigerPlayers").asInstanceOf[String]).asInstanceOf[Array[Dynamic]];
  dom.console.log(playerInfos);
  //  playerInfos.forEach(new Function1[Dynamic, Unit] {
  //    def apply(x: Dynamic): Unit = dom.console.log(x)
  //  });
  implicit val space = new cp.Space()
  space.damping = 0.75
  space.gravity = (0, 0)

  var players = mutable.Buffer.empty[Player];
  var playerFocusIndex = 0

  val svgDoc = new dom.DOMParser().parseFromString(
    scala.js.bundle.apply(src).string,
    "text/xml"
  )
  //dom.console.log(svgDoc);
  val svgElement = svgDoc.getElementsByTagName("svg")(0).asInstanceOf[SVG]
  //dom.console.log(svgElement);
  val xmlTree = Xml.parse(svgElement)(0)

  println("Static!")
  val staticShapes =
    xmlTree.get("Static").toSeq
      .flatMap(_.children)
      .flatMap(Form.processElement(_, static = true))


  println("Dynamic!")
  val dynamicShapes =
    xmlTree.get("Dynamic").toSeq
      .flatMap(_.children)
      .flatMap(Form.processElement(_, static = false))

  val widest = new cp.Vect(svgElement.width.baseVal.value, svgElement.height.baseVal.value)

  val backgroundImg = {
    dom.ext.Image.createBase64Svg(
      dom.btoa(
        s"<svg xmlns='http://www.w3.org/2000/svg' width='${widest.x}' height='${widest.y}'>" +
          new dom.XMLSerializer().serializeToString(svgDoc.getElementById("Background")) +
          "</svg>"
      )
    )
  }
  println("Clouds")
  val clouds = new Clouds(widest)

  println("staticJoints")
  val staticJoints =
    xmlTree.get("Joints").toSeq
      .flatMap(_.children)
      .collect { case x: Xml.Circle => x}
      .flatMap(Form.processJoint)

  println("player")
  val playerInfoSelected: Dynamic = playerInfos(players.size)
  //dom.console.log("Info !!!!!!");
  //dom.console.log(playerInfoSelected);



  println("Goal")
  val goal = new Goal(
    Form.processElement(xmlTree("Special")("Goal"), static = true).head
  )
  space.addCollisionHandler(1, 2, null, (arb: cp.Arbiter, space: cp.Space) => {
    if (arb.getB().getCollisionType() == 1) arb.getB().setCollisionType(3)
    if (arb.getA().getCollisionType() == 1) arb.getA().setCollisionType(3)
    goal.hit()
  }, null)
  space.addCollisionHandler(1, 3, null, (arb: cp.Arbiter, space: cp.Space) => {

    if (arb.getB().getCollisionType() == 1) arb.getB().setCollisionType(0)
    if (arb.getA().getCollisionType() == 1) arb.getA().setCollisionType(0)
    if (players.size>1 && players.forall(p => p.dead || p.isTiger)&& players.filter(p=>p.isTiger).size==1)
      goal.won = true;
  }, null)

  //val strokes = new Strokes(space)

  //  val lasers = new Lasers(
  //    player = player.form,
  //    laserElements = xmlTree.get("Lasers").toSeq
  //                           .flatMap(_.children)
  //                           .collect{case el: Xml.Line if el.misc.stroke == "#FF0000" => el},
  //    query = space.segmentQueryFirst(_, _, _, 0),
  //    pointQuery = space.pointQueryFirst(_, _, 0),
  //    kill = if (player.dead == 0.0) player.dead = 1.0
  //  )

  val fields: Seq[Field] = for {
    fieldElem <- xmlTree.get("Fields").toSeq
    beamElements = fieldElem.children
    (directions, fields) = beamElements.partition(_.isInstanceOf[Xml.Line])
    elem <- fields
  } yield {

    val (center, drawable, shape, area) = elem match {
      case Xml.Polygon(pts, misc) => (
        (0.0, 0.0),
        Drawable.Polygon(pts),
        new cp.PolyShape(space.staticBody, Form.flatten2(pts), (0, 0)),
        cp.Cp.areaForPoly(Form.flatten2(pts))
        )
      case Xml.Circle(x, y, r, misc) => (
        (x, y),
        Drawable.Circle(r),
        new cp.CircleShape(space.staticBody, r, (x, y)),
        math.Pi * r * r
        )
    }
    space.addShape(shape)
    shape.layers = Layers.Fields
    val vects: Seq[cp.Vect] = for {
      dir0 <- directions
      dir = dir0.asInstanceOf[Xml.Line]
      start = new cp.Vect(dir.x1, dir.y1)
      end = new cp.Vect(dir.x2, dir.y2)
      middle = (start + end) / 2
      res = shape.pointQuery(middle)
      if res.isDefined
    } yield {
      val d = end - start
      d / d.length
    }
    val Seq(acceleration, drag, _) =
      Form.splitColor(elem.misc.fill)
        .map(_ / 255.0)

    Field(
      center,
      drawable,
      shape,
      acceleration * 2,
      (drag - 0.5) * 2,
      vects.reduceOption(_ + _).fold(new cp.Vect(0, 0))(_ / vects.length)
    )
  }
  println("antigravity")
  val antigravity = new Antigravity(
    fields,
    query = space.shapeQuery(_, _),
    pointQuery = space.pointQueryFirst(_, _, 0)
  )
  println("camera")
  var camera: Camera = new Camera.Pan(
    initialDims,
    widest,
    checkpoints = List(
      (goal.p, 1.0),
      (widest / 2, math.min(initialDims.x / widest.x, initialDims.y / widest.y))
    ),
    finalCamera = new Camera.Follow(
      initialDims,
      if (goal.won|| players.size==0) goal.p
      else players(playerFocusIndex % players.size).form.body.getPos() + players(playerFocusIndex % players.size).form.body.getVel(),
      widest,
      0.5
    )
  )


  def draw(viewPort: cp.Vect, ctx: dom.CanvasRenderingContext2D) = {
    ctx.fillStyle = "#82CAFF"
    ctx.fillRect(0, 0, viewPort.x, viewPort.y)
    //strokes.drawStatic(ctx, viewPort.x, viewPort.y)
    camera.transform(ctx, viewPort) { ctx =>
      ctx.lineCap = "round"
      ctx.lineJoin = "round"
      clouds.draw(ctx)
      ctx.drawImage(backgroundImg, 0, 0)

      for (form <- staticShapes ++ dynamicShapes if form != null) {
        Util.draw(ctx, form)
      }
      val activePlayerIndex = playerFocusIndex % players.size
      //lasers.draw(ctx)
      goal.draw(ctx)
      for ((player, n) <- players.zipWithIndex) {
        if (n != activePlayerIndex)
          player.draw(ctx)
      }
      if (players(activePlayerIndex).dead) playerFocusIndex = playerFocusIndex + 1
      players(activePlayerIndex) draw(ctx, true)
      antigravity.draw(ctx)


      staticJoints.foreach { jform =>
        ctx.save()
        ctx.fillStyle = jform.fillStyle.toString
        ctx.strokeStyle = jform.strokeStyle.toString
        ctx.translate(jform.joint.a.getPos().x, jform.joint.a.getPos().y)
        ctx.rotate(jform.joint.a.a)
        ctx.beginPath();
        ctx.arc( jform.joint.anchr1.x+5, jform.joint.anchr1.y+5, 5, 0, 2*Math.PI, false );
        ctx.stroke();
        ctx.closePath();
        ctx.fill();
        ctx.restore()
      }

      //strokes.draw(ctx)
    }
    // strokes.drawStatic(ctx, viewPort.x, viewPort.y)
    goal.drawFade(ctx)

  }




  def update(input: Level.Input): Level.Result = {

    if (input.keys(KeyCode.escape)) Level.Result.Exit
    else {
      camera.update(input.keys, input.screenSize)
      clouds.update()

      //.update(input.keys)
      def screenToWorld(p: cp.Vect) = ((p - input.screenSize / 2) / camera.scale) + camera.pos
      def bestMoveForBot():cp.Vect={
        screenToWorld(goal.goal.body.getPos())
      }
      //dom.console.log(playerInfoSelected.intelligenceLevel)

      if(players.size==0)
      players = players :+ Player(
        Form.processElement(
          xmlTree("Special")("Player"),
          static = false
        ).head,
        widest = widest,
        players.size,
        playerInfoSelected.name.asInstanceOf[String],
        playerInfoSelected.color.asInstanceOf[String],
        playerInfoSelected.isHuman.asInstanceOf[Boolean],
        bestMoveForBot,
        playerInfoSelected.intelligenceLevel.asInstanceOf[String].toInt
      )
      val lastTouch = if (input.touches.size > 0) {
        input.touches.last match {
          case Touch.Down(x) => Touch.Down(screenToWorld(x))
          case Touch.Move(x) => Touch.Move(screenToWorld(x))
          case Touch.Up(x) => Touch.Up(screenToWorld(x))
        }
      }
      else null;
      val selectedPlayer = players(playerFocusIndex % players.size)
      selectedPlayer.update(lastTouch)

      // def stillMoving = players.exists(p => Math.abs(p.form.body.getVel().x) > 100 || Math.abs(p.form.body.getVel().y) > 100 || Math.abs(p.form.body.a) > 25)
      // if (Math.abs(selectedPlayer.form.body.getVel().x) < 130 && Math.abs(selectedPlayer.form.body.getVel().y) <130 && Math.abs(selectedPlayer.form.body.a) <30)
      if (!selectedPlayer.getSupressFlag)
        lastTouch match {
          case Touch.Up(x) =>
            selectedPlayer.setSupressFlag(true)
            def codeBlock= () => {
              if (playerFocusIndex < playerInfos.size - 1) {
                val playerInfoSelected: Dynamic = playerInfos(players.size)
                players = players :+ Player(
                  Form.processElement(
                    xmlTree("Special")("Player"),
                    static = false
                  ).head,
                  widest = widest,
                  players.size,
                  playerInfoSelected.name.asInstanceOf[String],
                  playerInfoSelected.color.asInstanceOf[String],
                  playerInfoSelected.isHuman.asInstanceOf[Boolean],
                  bestMoveForBot,
                  playerInfoSelected.intelligenceLevel.asInstanceOf[String].toInt
                )
              }

              //players.map(p=>p.setSupressFlag(false));
              selectedPlayer.setSupressFlag(false)
              playerFocusIndex = playerFocusIndex + 1;
//              input.touches.last match {
//                case Touch.Down(x) => players(playerFocusIndex % players.size).update(Touch.Down(screenToWorld(x)))
//                case Touch.Move(x) => players(playerFocusIndex % players.size).update(Touch.Down(screenToWorld(x)))
//                case Touch.Up(x) => players(playerFocusIndex % players.size).update(Touch.Down(screenToWorld(x)))
//              }
              if(players.size>1&&players.forall(p=>p.dead||p.isTiger)){
                players.filter(p=>p.isTiger).map(p=>p.form.shapes.map(_.setCollisionType(1)))
              }
            }

            //            while(playerFocusIndex>=numberOfPlayer&& players(playerFocusIndex%players.size).dead)
            //              playerFocusIndex = playerFocusIndex + 1
            dom.setTimeout(codeBlock, 3000)

          case _ => {}
        }

      if (goal.update()) Level.Result.Next
      else {
        antigravity.update()
        space.step(1.0 / 60)
        //lasers.update()

        draw(input.screenSize, input.painter)
        this
      }
    }

  }

}
