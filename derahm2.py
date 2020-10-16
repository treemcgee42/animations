from manimlib.imports import *
from NumberCreature.NumberCreature import *

class title(Scene):
    def construct(self):
        Ale = Alex().scale(.75)
        Ale[4].set_color(GREEN)

        self.wait()
        self.play(   FadeIn(Ale))
        wh = "Why?"
        self.play(  Blink(Ale))
        self.play(  NumberCreatureSays(
            Ale, wh, target_mode="thinking", bubble_class=ThoughtBubble,\
            run_time=4
            )
        )
        self.wait(1)
        self.play(  Blink(Ale))
        self.wait(1)
        self.play(  Blink(Ale))
        self.wait(1)
        self.play(  Blink(Ale))
        self.wait(1)
        self.play(  Blink(Ale))
        self.wait(1)
        self.play(  Blink(Ale))
        self.wait(1)
        self.play(  Blink(Ale))
        self.wait(1)
        self.play(  Blink(Ale))
        self.wait(1)
        self.play(  RemovePiCreatureBubble(Ale), FadeOut(Ale))
        self.wait()

        cohomology = TextMobject("Cohomology").scale(1.5).shift(.75*UP)
        derahm = TextMobject("de Rahm Cohomology").next_to(cohomology,\
                        direction = DOWN, buff=SMALL_BUFF).scale(.75)
        part1 = TextMobject("Part 1: The Idea").next_to(derahm,\
                        direction = DOWN, buff=SMALL_BUFF).scale(.75)

        self.play(  Write(cohomology))
        self.wait()
        self.play(  Write(derahm))
        self.wait()
        self.play(  FadeIn(part1))
        self.wait()

class prevsaw(Scene):
    def construct(self):
        hom = TextMobject("Homology:").shift(2*LEFT,.75*UP)
        hominfo = TextMobject("\\textit{detects }", "holes").next_to(hom,\
                            buff=1.25)
        hominfo[0].set_color(RED)
        cohom = TextMobject("Cohomology:").next_to(hom, direction=DOWN,\
                            buff=MED_LARGE_BUFF)
        cohominfo = TextMobject("\\textit{measures }", "holes").next_to(cohom,\
                            buff=LARGE_BUFF)
        cohominfo[0].set_color(RED)

        self.play(  Write(hom))
        self.play(  FadeIn(hominfo))
        self.wait()
        self.play(  Write(cohom))
        self.play(  FadeIn(cohominfo))
        self.wait(3)

        self.play(  *[FadeOut(mob)for mob in self.mobjects])

class manifolds(Scene):
    def construct(self):
        mani = ImageMobject("manifold.png").scale(2)
        credit = TextMobject("\\tiny{Credit: Wikimedia Commons}").to_edge(DOWN, buff=MED_SMALL_BUFF)\
                        .to_edge(RIGHT,buff=MED_SMALL_BUFF)
        self.wait()
        self.play( FadeIn(mani), FadeIn(credit))
        self.wait(5)
        self.play(  FadeOut(mani), FadeOut(credit))
        self.wait()

class ftc(Scene):
    def construct(self):
        Ale = Alex().scale(.75)
        Ale[4].set_color(GREEN)
        ftc = TextMobject("The Fundamental Theorem of Calculus").shift(2*UP)
        self.play(  GrowFromCenter(Ale))
        self.wait(1)
        self.play(  Blink(Ale))
        self.wait(1)
        self.play(  Write(ftc), Blink(Ale))
        self.wait(1)
        self.play(  Blink(Ale))
        self.wait(1)
        self.play(  FadeOut(Ale), FadeOut(ftc))
        self.wait()

class terry(Scene):
    def construct(self):
        tt = ImageMobject("tt.jpg").to_edge(LEFT, buff=LARGE_BUFF)
        quote = TextMobject(
            "The integration on forms concept is of fundamental importance in differential \
            topology, geometry, and physics, and also yields one of the most important \
            examples of cohomology, namely", " de Rahm cohomology", ", which (roughly \
            speaking) measures precisely the extent to which the fundamental \
            theorem of calculus fails in higher dimensions and on general \
            manifolds.",
            alignment=""
        )
        quote = VGroup(*it.chain(*quote))
        quote.set_width(FRAME_WIDTH - tt.get_width() - 2)
        quote.to_edge(RIGHT)

        quote_rect = SurroundingRectangle(quote, buff=MED_SMALL_BUFF)
        quote_rect.set_stroke(WHITE, 2)
        quote_rect.stretch(1.1, 1)

        who = TextMobject("-Terrence Tao, \\textit{Differential Forms and Integration}")\
                .scale(.5).next_to(quote_rect, DOWN).shift(.35*UP).to_edge(RIGHT, buff=MED_SMALL_BUFF)


        self.play(  GrowFromCenter(tt))
        self.wait()
        self.play(  Write(quote, run_time=5))
        self.play(  FadeIn(who))
        self.wait(3)
        self.play(  FadeOut(tt), FadeOut(who), FadeOut(quote))
        self.wait()

class langdiffforms(Scene):
    def construct(self):
        Ale = Alex().shift(.5*DOWN).scale(.75)
        Ale[4].set_color(GREEN)

        self.wait()
        self.play(  GrowFromCenter(Ale))
        self.wait(1)
        self.play(  Blink(Ale))
        self.wait(1)

        diff = TextMobject("Differential Forms").next_to(Ale,\
                    direction=UP, buff=MED_LARGE_BUFF)

        self.play(  Write(diff), Blink(Ale))
        self.wait(1)
        self.play(  Blink(Ale))
        self.wait(1)
        self.play(  FadeOut(diff), Blink(Ale))
        self.wait(1)
        self.play(  Blink(Ale))
        self.play(  FadeOut(Ale))
        self.wait()

class alpha1(GraphScene):
    CONFIG = {
        "y_max" : 10,
        "y_min" : 0,
        "x_max" : 10,
        "x_min" : 0,
        "axes_color" : BLUE,
        "x_tick_frequency" : 10,
        "y_tick_frequency" : 10
    }
    def construct(self):
        self.setup_axes(animate=True)
        self.wait(4)
        graph = self.get_graph(lambda x : 2*(np.sin((x-2)/2))+5,  
                                    color = GREEN,
                                    x_min = 2, 
                                    x_max = 2+2*PI
                                    )
        self.play(
        	ShowCreation(graph),
            run_time = 2
        )
        self.wait()

        daxis = Dot(self.coords_to_point(2,0))
        dcurve1 = Dot(graph.points[0])

        self.play(  FadeIn(daxis), FadeIn(dcurve1))
        self.add(   daxis.copy(), dcurve1.copy())

        def val(obj):
            obj.move_to((dcurve1.get_center()[0],\
                    self.coords_to_point(0,0)[1],0))
        daxis.add_updater(val)

        # colored line
        dcop = daxis.copy()
        l = Line(dcop, daxis)
        l.add_updater(  lambda m:
            m.become(   Line(dcop, daxis, color=RED))
        )
        self.add(   l)


        self.play(  MoveAlongPath(dcurve1,graph,run_time=4))
        self.wait(3)

        graph2 = self.get_graph(lambda x : -2*(np.sin((x-2)/2))+5,  
                                    color = GREEN,
                                    x_min = 2+2*PI, 
                                    x_max = 2
                                    )

        self.play(  ShowCreation(graph2))
        self.play(  MoveAlongPath(dcurve1,graph2,run_time=4))
        self.wait(2)
        self.play(  *[FadeOut(mob)for mob in self.mobjects])

class alpha2(GraphScene):
    def construct(self):
        yax = Line((0,-3.5,0),(0,3.5,0))
        xax = Line((-6,0,0), (6,0,0))

        self.wait()
        self.play(  ShowCreation(yax), ShowCreation(xax))
        self.wait()

        func = lambda p: np.array([
            p[1],
            0,
            0
        ])
        
        vf = VectorField(func)
        self.play(*[GrowArrow(vec) for vec in vf])
        self.wait()

        c = Circle(color=WHITE, stroke_width = 3).move_to((2,2,0))

        self.play(  ShowCreation(c))

        cdot = Dot( tuple(c.points[0]))
        caxis = Dot( tuple([c.points[0][1],0,0]))

        self.play(  ShowCreation(cdot), ShowCreation(caxis))
        self.add(   cdot.copy())

        def val(obj):
            obj.move_to((cdot.get_center()[1],0,0))
        caxis.add_updater(val)

        # Val integral

        def ival( point_to ):
            return point_to.get_center()[1]
        integral = ValueTracker((c.points[0][1]))
        i_tex = DecimalNumber(integral.get_value())
        i_tex = DecimalNumber(integral.get_value()).add_updater(
            lambda v: v.set_value((i_tex.get_value()+ival(cdot)) if ival(cdot)>=c.points[0][1]
                else (i_tex.get_value()-ival(cdot))
            )
        )

        # text

        r = RoundedRectangle(fill_color=BLACK, fill_opacity = .75,\
                        color=BLACK, opacity = 0).shift(3*LEFT,1.75*UP)
        vftex = TexMobject("(y,0)").shift(3*LEFT,2*UP)
        iexp = TextMobject("\\small{Value of integral:}").shift(3.5*LEFT,1*UP)\
            .scale(.75)
        i_tex.next_to(iexp,buff=SMALL_BUFF).scale(.75)
        
        self.play(FadeIn(r), FadeIn(i_tex), FadeIn(iexp), FadeIn(vftex))
        self.play(  MoveAlongPath(cdot,c,run_time=4))
        self.wait(2)
        self.play(  *[FadeOut(mob)for mob in self.mobjects])

class nodiff(Scene):
    def construct(self):
        Ale = Alex().scale(.75)
        Ale[4].set_color(GREEN)

        self.wait()
        self.play(  GrowFromCenter(Ale))
        self.wait(1)
        self.play(  NumberCreatureSays(Ale, "?",target_mode="thinking", bubble_class=ThoughtBubble),\
                    Blink(Ale))
        self.wait(1)
        self.play(  Blink(Ale))
        self.wait(1)
        self.play(  Blink(Ale))
        self.wait(1)

        proj = TexMobject("\\operatorname{proj}_x").shift(2*DOWN,2*LEFT)
        vf = TexMobject("(y,0)").shift(2*DOWN,2*RIGHT)

        self.play(  FadeIn(proj), Blink(Ale))
        self.play(  FadeIn(vf))
        self.play(  Blink(Ale))
        self.wait(1)
        self.play(  FadeOut(proj), FadeOut(vf), RemovePiCreatureBubble(Ale), FadeOut(Ale))
        self.wait()

class alpha3(Scene):
    def construct(self):
        a = Annulus(inner_radius = .25, outer_radius = 3, fill_opacity=0,\
                    stroke_width = 3)

        sc = Circle(radius = .5, fill_opacity=0, stroke_width=3,\
                    color=BLUE).move_to((-1,1,0))

        self.wait()
        self.play(  ShowCreation(a))
        self.wait(3)
        self.play(  ShowCreation(sc))
        self.wait()

        l = Line((0,0,0), sc.points[0])
        dl = DashedLine((0,0,0), (3,0,0)).rotate_about_origin(l.get_angle())

        ptsmallcirc = Dot(l.points[-1], radius = .07)

        def val(obj):
            obj.set_angle(Line((0,0,0), ptsmallcirc.get_center()).get_angle())
        dl.add_updater(val)

        # colored arc

        dlcopy = dl.copy()

        asc = Arc(radius=3, start_angle=dlcopy.get_angle(),\
                    angle = dl.get_angle()- dlcopy.get_angle(), color=GREEN)

        asc.add_updater(
            lambda m: m.become(
                Arc(
                    radius=3,
                    start_angle=dlcopy.get_angle(),
                    angle = dl.get_angle()- dlcopy.get_angle(),
                    color=GREEN
                )
            )
        )

        self.play(  ShowCreation(ptsmallcirc), ShowCreation(dl),\
                    ShowCreation(asc))
        self.wait(3)
        self.play(  MoveAlongPath(ptsmallcirc, sc, run_time=4))
        self.wait()
        self.play(  FadeOut(ptsmallcirc), FadeOut(dl), FadeOut(sc))
        dl.remove_updater(val)
        asc.clear_updaters()

        bc = Circle(radius = 1.5, fill_opacity=0, stroke_width=3,\
                    color=BLUE)

        self.play(  ShowCreation(bc))

        l2 = Line((0,0,0), bc.points[0])
        dl2 = DashedLine((0,0,0), (3,0,0)).rotate_about_origin(l2.get_angle())

        ptbigcirc = Dot(l2.points[-1], radius = .07)

        def val2(obj):
            obj.set_angle(Line((0,0,0), ptbigcirc.get_center()).get_angle())
        dl2.add_updater(val2)

        # colored arc

        dl2copy = dl2.copy()

        abc = Arc(radius=3, start_angle=0,\
                    angle = 0, color=GREEN)

        abc.add_updater(
            lambda m: m.become(
                Arc(
                    radius=3,
                    start_angle=0,
                    angle = dl2.get_angle() if dl2.get_angle()>=0\
                            else TAU+dl2.get_angle(),
                    color=GREEN
                )
            )
        )

        self.play(  ShowCreation(ptbigcirc), ShowCreation(dl2),\
                    ShowCreation(abc))
        self.play(  MoveAlongPath(ptbigcirc, bc, run_time=4))
        self.wait(2)
        self.play(  *[FadeOut(mob)for mob in self.mobjects])

class closedforms(Scene):
    def construct(self):
        Ale = Alex().scale(.75).shift(4*LEFT)
        Ale[4].set_color(GREEN)

        self.wait()
        self.play(  GrowFromCenter(Ale))
        self.wait(1)

        ic = TextMobject("Integrals valued over any ``small'' curve are 0.").scale(.75)\
                .shift(1*RIGHT)
        self.play(  Blink(Ale), Write(ic))
        self.wait(1)
        self.play(  Blink(Ale))
        self.wait(1)
        self.play(  FadeOut(Ale), FadeOut(ic))
        self.wait()

        cf = TextMobject("Closed forms:").set_color(RED).shift(4.5*LEFT,2*UP)
        definition = TextMobject("Differential forms $f$ whose exterior derivative $df$ is 0.",\
                        alignment="")\
                        .next_to(cf, direction=DOWN, buff=MED_SMALL_BUFF).shift(4.35*RIGHT)

        self.play(  Write(cf))
        self.wait()
        self.play(  FadeIn(definition))
        self.wait(3)

        coorig = ImageMobject("cochainorig.png").scale(.5).shift(1.5*DOWN)
        cowithdiffforms = ImageMobject("cochaindiff.png").scale(.5).shift(1.5*DOWN)
        cocomplete = ImageMobject("cochainderahm.png").scale(.5).shift(1.5*DOWN)

        self.play(  FadeIn(coorig))
        self.wait(2)
        self.play(  Transform(coorig, cowithdiffforms))
        self.remove(    coorig)
        self.wait(2)
        self.play(  Transform(cowithdiffforms, cocomplete))
        self.wait(2)
        self.play(  *[FadeOut(mob)for mob in self.mobjects])

class vspacecf(Scene):
    def construct(self):
        Ale = Alex().scale(.75).shift(.5*UP)
        Ale[4].set_color(GREEN)
        c = TextMobject("Closed forms are irrotational.").next_to(Ale, direction=DOWN,\
                buff= MED_SMALL_BUFF)

        self.play(  GrowFromCenter(Ale))
        self.wait(1)
        self.play(  Blink(Ale))
        self.wait(1)
        self.play(  Write(c), Blink(Ale))
        self.wait(1)
        self.play(  FadeOut(c), FadeOut(Ale))
        self.wait()

class checkcf(Scene):
    def construct(self):
        a1 = ImageMobject("alpha1.png").scale(1.5).shift(3*LEFT,2.1*UP)
        a2 = ImageMobject("alpha2.png").scale(1.25).shift(3*RIGHT,2.2*UP)
        a3 = ImageMobject("alpha3.png").scale(1.5).shift(1.75*DOWN)

        a1label = TextMobject("Case 1").scale(.75).next_to(a1, direction=DOWN,\
                    buff=0)
        a2label = TextMobject("Case 2").scale(.75).next_to(a2, direction=DOWN,\
                    buff=.3)
        a3label = TextMobject("Case 3").scale(.75).next_to(a3, direction=DOWN,\
                    buff=0)

        self.play(  GrowFromCenter(a1), GrowFromCenter(a1label),\
                    GrowFromCenter(a2), GrowFromCenter(a2label),\
                    GrowFromCenter(a3), GrowFromCenter(a3label))
        self.wait(3)
        self.play(  Transform(a2, a2.copy().set_opacity(.5)),\
                    Transform(a2label, a2label.copy().set_opacity(.5)))
        self.wait(3)
        self.play(  *[FadeOut(mob)for mob in self.mobjects])
        self.wait()

class firstcase(Scene):
    def construct(self):
        fc = TextMobject("First case:").to_edge(LEFT, buff=1)\
                .to_edge(UP, buff=1)

        yax = Line((0,-3,0),(0,3,0))
        xax = Line((-3,0,0), (3,0,0))

        self.wait()
        self.play(  ShowCreation(yax), ShowCreation(xax), Write(fc))
        self.wait(3)

        func = lambda p: np.array([
            p[0],
            0,
            0
        ])
        
        vf = VectorField(func, x_min=-3, x_max=3, y_min=-3, y_max=3)
        self.play(*[GrowArrow(vec) for vec in vf])
        self.wait(2)

        self.play(  *[FadeOut(mob)for mob in (self.mobjects[:2]+self.mobjects[3:])])
        self.wait(2)

        f = TexMobject("f(x)=x").shift(1*UP)
        df = TexMobject("\dfrac{df}{dx}=1").next_to(f,buff=MED_SMALL_BUFF, direction=DOWN)
        dx = TexMobject("df=dx").next_to(df,buff=MED_SMALL_BUFF, direction=DOWN)
        ddx = TexMobject("d(dx)=0").next_to(dx,buff=MED_SMALL_BUFF, direction=DOWN)

        self.play(  Write(f))
        self.play(  Write(df))
        self.play(  Write(dx))
        self.wait()
        self.play(  Write(ddx))
        self.wait()
        self.play(  *[FadeOut(mob)for mob in self.mobjects])
        self.wait()

class thirdcase(Scene):
    def construct(self):
        tc = TextMobject("Third case:").to_edge(LEFT, buff=1)\
                .to_edge(UP, buff=1)
        a3 = ImageMobject("alpha3.png").scale(2)

        self.wait()
        self.play(  Write(tc))
        self.play(  FadeIn(a3))
        self.wait(3)

        func = lambda p: np.array([
           ((Line((0,0,0),p)).get_angle() if (Line((0,0,0),p)).get_angle() >=0\
                else (TAU+(Line((0,0,0),p)).get_angle())),
            0,
            0
        ])
        
        vf = VectorField(func, x_min=-3, x_max=3, y_min=-3, y_max=3)
        self.play(*[GrowArrow(vec) for vec in vf])
        self.wait(2)
        self.play(  *[FadeOut(mob)for mob in self.mobjects[1:]])
        self.wait(2)
        
        Ale = Alex().scale(.75)
        Ale[4].set_color(GREEN)

        self.play(  GrowFromCenter(Ale))
        self.wait(1)
        self.play(  Blink(Ale))
        self.wait(1)
        self.play(  Blink(Ale))
        self.wait(1)
        self.play(  Blink(Ale))
        self.wait(1)
        self.play(  FadeOut(Ale))
        self.wait()

        fnot = TextMobject("$f$"," is not globally continuous,").shift(.5*UP)
        fnot[0].set_color(RED)
        fnot2 = TextMobject("but since it is continuous locally, ","$d\\theta$"," exists.")\
                    .next_to(fnot,direction=DOWN, buff=MED_SMALL_BUFF)
        fnot2[1].set_color(RED)

        self.play( Write(fnot))
        self.play(  Write(fnot2))
        self.wait(2)
        self.play(  FadeOut(fnot), FadeOut(fnot2))
        self.wait()

        dtheta = TexMobject("d\\theta=\\dfrac{-y\\ dx+x\\ dy}{x^2+y^2}").shift(.5*UP)
        ddtheta = TexMobject("d\\ d\\theta=0").next_to(dtheta,direction=DOWN,\
                    buff=MED_SMALL_BUFF)
        self.play(  Write(dtheta))
        self.wait()
        self.play(  Write(ddtheta))
        self.wait(2)
        self.play(  *[FadeOut(mob)for mob in self.mobjects])

class secondcase(Scene):
    def construct(self):
        sc = TextMobject("Second case:").to_edge(LEFT, buff=1)\
                .to_edge(UP, buff=1)

        yax = Line((0,-3,0),(0,3,0))
        xax = Line((-3,0,0), (3,0,0))

        self.wait()
        self.play(  ShowCreation(yax), ShowCreation(xax), Write(sc))
        self.wait(3)

        func = lambda p: np.array([
            p[1],
            0,
            0
        ])
        
        vf = VectorField(func, x_min=-3, x_max=3, y_min=-3, y_max=3)
        self.play(*[GrowArrow(vec) for vec in vf])
        self.wait(2)
        self.play(  ShowCreation(xax.copy().set_color(PURPLE)))
        self.wait(2)
        self.play(  *[FadeOut(mob)for mob in (self.mobjects[:2]+self.mobjects[3:])])
        self.wait(2)

        bef = TexMobject("f(x,y)=(y,0)").shift(1.5*UP)
        oneform = TexMobject("y\\textbf{i}+0\\textbf{j}",\
                    "\longleftrightarrow","y\\ dx+0\\ dy").next_to(bef,direction=DOWN,\
                    buff=MED_SMALL_BUFF)
        oneform2 = TexMobject("df = \\left( \\dfrac{\\partial f_y}{\\partial x}\
                    -\\dfrac{\\partial f_x}{\\partial y}\\right)dx\\wedge dy").next_to(oneform,direction=DOWN,\
                    buff=MED_SMALL_BUFF)
        oneform3 = TexMobject("df=-dx\wedge dy").next_to(oneform2,direction=DOWN,\
                    buff=MED_SMALL_BUFF)
        fin = TexMobject("\\neq 0").set_color(RED).next_to(oneform3,direction=DOWN,\
                    buff=MED_SMALL_BUFF)

        self.play(  FadeIn(bef))
        self.wait()
        self.play(  Write(oneform))
        self.play(  Write(oneform2))
        self.play(  Write(oneform3))
        self.wait()
        self.play(  FadeIn(fin))
        self.wait(2)
        self.play(  *[FadeOut(mob)for mob in self.mobjects])
        self.wait()

class exactforms(Scene):
    def construct(self):
        Ale = Alex().scale(.5).shift(2*DOWN)
        Ale[4].set_color(GREEN)

        a1 = ImageMobject("alpha1.png").shift(2.5*LEFT,2*UP).scale(1.5)
        a3 = ImageMobject("alpha3.png").shift(2.5*RIGHT,2*UP).scale(1.5)

        self.play(  GrowFromCenter(Ale))
        self.wait(1)
        self.play(  Blink(Ale))
        self.wait(1)
        self.play(  FadeIn(a1), FadeIn(a3), Blink(Ale))
        self.wait(1)
        self.play(  Blink(Ale))
        self.wait(1)
        self.play(  *[FadeOut(mob)for mob in self.mobjects])
        self.wait()

        ef = TextMobject("Exact forms:", alignment="").set_color(RED).shift(1.5*UP,3.75*LEFT)
        efdef = TextMobject("Forms which are the derivative of another form.", alignment="")\
                    .next_to(ef,direction=DOWN,buff=MED_SMALL_BUFF).shift(3.9*RIGHT)

        self.play(  Write(ef))
        self.wait()
        self.play(  FadeIn(efdef))
        self.wait(2)

        a1.move_to((-3,-2,0))
        self.play(  GrowFromCenter(a1))
        self.wait()

        dxlone = TexMobject("dx").shift(3*RIGHT,.5*DOWN).set_color(GREEN)
        a1grad1 = TexMobject("=\\left( \\dfrac{\partial(x)}{\partial x}dx+\
                    \dfrac{\partial(x)}{\partial y}dy\\right)")\
                    .next_to(dxlone,direction=DOWN,buff=MED_SMALL_BUFF)
        a1grad2 = TexMobject("=d(","x",")")\
                    .next_to(a1grad1,direction=DOWN,buff=MED_SMALL_BUFF)
        a1grad2[1].set_color(RED)

        self.play(  Write(dxlone))
        self.wait()
        self.play(  Write(a1grad1))
        self.play(  Write(a1grad2))
        self.wait(2)

        isexact = TextMobject("Exact").set_color(RED).shift(3*RIGHT,1.5*DOWN)
        self.play(  *[FadeOut(mob)for mob in [dxlone,a1grad1,a1grad2]],\
                    FadeIn(isexact))
        self.wait(2)

        a3.move_to((-3,-1.75,0))
        self.play(  FadeOut(isexact), Transform(a1,a3))
        self.wait(2)

        dthetalone = TexMobject("d\\theta").shift(3*RIGHT,1.5*DOWN).set_color(GREEN)
        a3grad = TexMobject("=d(","?",")")\
                    .next_to(dthetalone,direction=DOWN,buff=MED_SMALL_BUFF)
        a3grad[1].set_color(RED)

        self.play(  Write(dthetalone))
        self.wait()
        self.play(  Write(a3grad))
        self.wait(2)
        
        self.wait(2)

        isnotexact = TextMobject("Not exact").set_color(RED).shift(3*RIGHT,1.5*DOWN)
        self.play(  *[FadeOut(mob)for mob in [dthetalone,a3grad]],\
                    FadeIn(isnotexact))
        self.wait(2)
        self.play(  *[FadeOut(mob)for mob in self.mobjects])
        self.wait()

class bringback(Scene):
    def construct(self):
        c1 = ImageMobject("cochainorig.png").scale(.5).shift(2*UP)
        c2 = ImageMobject("cochaindiff.png").scale(.5).move_to(c1)
        c3 = ImageMobject("cochainderahm.png").scale(.5).move_to(c2)

        self.wait()
        self.play(  GrowFromCenter(c1))
        self.wait()
        self.play(  Transform(c1,c2))
        self.wait()
        self.remove(c1)
        self.play(  Transform(c2,c3))
        self.wait(2)

        cohomdef = TexMobject("H^1_{\\operatorname{dR}}=\\dfrac{\\operatorname{ker}\
                    \\ d}{\\operatorname{im}\\ d")
        intdef = TexMobject("=\dfrac{\mbox{\{closed 1-forms\} }}{\mbox{\{exact 1-forms\}}}")\
                    .next_to(cohomdef,direction=DOWN,buff=MED_SMALL_BUFF)

        self.play(  Write(cohomdef))
        self.wait()
        self.play(  Write(intdef))
        self.wait(2)
        self.play(  *[FadeOut(mob)for mob in self.mobjects])
        self.wait()

class nodetails(Scene):
    def construct(self):
        c1 = TextMobject("Case 1: $\mathbb{R}^2$")\
                .to_edge(LEFT,buff=1).to_edge(UP,buff=1)
        c3 = TextMobject("Case 3: $\mathbb{R}^2\\setminus \{0\}$")\
                .to_edge(LEFT,buff=1).to_edge(UP,buff=1)

        a1 = ImageMobject("alpha1.png").scale(1.5).shift(3*RIGHT,2*UP)
        a3 = ImageMobject("alpha3.png").scale(1.5).move_to(a1)

        ca1 = TexMobject("H^i_{\operatorname{dR}}=\\begin{cases}\
                0&\\forall i\\end{cases}").shift(2*LEFT)
        ca3 = TexMobject("H^i_{\operatorname{dR}}=\\begin{cases}\
                \mathbb{R}& i=0,1\\\ 0& i>1\\end{cases}").shift(3*LEFT)

        Ale = Alex().scale(.75).shift(3*RIGHT,2*DOWN)
        Ale[4].set_color(GREEN)

        self.wait()
        self.play(  GrowFromCenter(Ale))
        self.wait(1)
        self.play(  Blink(Ale), Write(c1), FadeIn(a1))
        self.wait(1)
        self.play(  Blink(Ale))
        self.wait(1)
        self.play(  Blink(Ale), Write(ca1))
        self.wait(1)
        self.play(  Blink(Ale))
        self.wait(1)
        self.play(  Blink(Ale))
        self.wait(1)
        self.play(  Transform(c1,c3), Transform(a1,a3), Transform(ca1,ca3))
        self.wait(1)
        self.play(  Blink(Ale))
        self.wait(1)
        self.play(  Blink(Ale))
        self.wait(1)
        self.play(  *[FadeOut(mob)for mob in self.mobjects])
        self.wait()
        
class roundout(Scene):
    def construct(self):
        todo = TextMobject("\
            \\begin{enumerate} \\item Develop intuition for differential forms\
            \\item Compute de Rahm groups \\item Connections to calculus\
            \\item Algebraic invariance \\end{enumerate} \
        ")
        self.play( Write(todo, run_time=5))
        self.wait(2)
        self.play(  *[FadeOut(mob)for mob in self.mobjects])
