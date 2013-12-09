---
title: d3.js + R with polycharts.js
date : 2012-12-14
--- &lead

[Polycharts](http://polychartjs.com/) is a charting library that allows users to create elegant, interactive visualizations using `d3.js` and grammar of graphics.

---

<script 
  src="https://raw.github.com/Polychart/polychart.js/master/polychart.latest.min.js">
</script>

<style>
.mycode pre {
  font-size: 11px;
  line-height: 16px;
}
</style>

<script>
gg.opts.graph.branding = false;
gg.opts.graph.width = 400;
gg.opts.graph.height = 300;
</script>

--- &poly chartid:chart1

<script>
gg.data.fetch("data/tips.csv", function(data){
  gg.graph(data)
    .layer(gg.layer.point()
      .map({'x': 'total_bill', 'y': 'tip'})
      .map('color', 'day')
      .opts({'opacity': 0.6, 'radius': 2}))
    .opts('title', 'Tips Dataset')
    .facet(gg.facet.wrap('sex').ncol(2))
    .render("#{{slide.chartid}}")
  });
</script>

--- &poly chartid:chart2

<script>
gg.data.fetch('data/mtcars.csv', function(data){
  gg.graph(data)
  .layer(gg.layer.bar()
    .map({'x':'cyl', 'y':'count', 'color':'gear'}))
  .scale('x', gg.scale.discrete())
  .render('#{{slide.chartid}}');
});
</script>

--- &poly chartid:chart3

<script>
  gg.data.fetch("data/iris.csv", function(data) {
    gg.graph(data)
    .layer(gg.layer.point()
      .map({'x':'SepalLength', 'y':'PetalLength'})
      .map('color', 'Species'))
    .opts('title', 'Iris: Scatter Plot')
    .render("#{{slide.chartid}}");
  }); 
</script>

--- &poly chartid:chart4

<script>
  gg.data.fetch("data/iris.csv", function(data){
    gg.graph(data)
    .layer(gg.layer.point()
      .map({'x': 'SepalLength', 'y': 'PetalLength'}))
    .opts('title', 'Iris Flower Dataset')
    .facet(gg.facet.wrap('Species').ncol(3))
    .render("#{{slide.chartid}}")
  });
</script>

---


```r
dat = setNames(iris, nm = gsub(".", "", names(iris), fixed = TRUE))
write.csv(dat, file = "data/iris.csv")
dat2 = plyr::ddply(mtcars, .(cyl, gear), summarise, count = length(cyl))
```

```
## Error: could not find function "."
```

```r
write.csv(dat2, file = "data/mtcars.csv")
```

```
## Error: object 'dat2' not found
```


<div class="alert">
 <p> Due to same-origin-policy, reading external data will not work for static files. You will have to serve the blog using a local server. I recommend the rubygem `white_castle` which does not require any configuration.</p>
</div>






