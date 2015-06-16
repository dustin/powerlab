var refreshInterval = 5000;
var maxStatuses = 3600;

var statuses = [];
var lastStatus = '';

var batterygauge;
var cellchart;

function sp(d) {
    return d.detected_cell_count + "S" + d.packs + "P";
}

function farray(a) {
    var rv = [];
    for (var i = 0; i < a.length; i++) {
        rv.push(a[i].toFixed(2));
    }
    return rv.join(" ");
}

/*
  "mode": "charging",
  "mode": "detecting pack",
  "mode": "discharging",
  "mode": "halt for safety",
  "mode": "monitoring",
  "mode": "pack cool down",
  "mode": "system stop",
*/

function updateStatus(dees) {
    if (dees.length == 0) {
        console.log("Returning with empty input");
        return;
    }

    $(".modesec").hide();

    statuses = statuses.concat(dees);
    statuses.sort(function(a, b) { return a.TS < b.TS ? -1 : 1; });
    if (statuses.length > maxStatuses) {
        statuses = statuses.splice(statuses.length-maxStatuses);
    }

    var last = dees[dees.length-1];
    var d = last.ST;
    lastStatus = last.TS;

    var mode = d.mode.replace(' ', '-');

    if (mode == 'charging' || mode == 'discharging' || mode == 'pack cool down') {
        d3.select("#mode").text(mode + " " + sp(d) + " " + d.avg_cell.toFixed(1) + "%");
        if (d.charge_complete) {
            $("#mode").addClass('complete');
        } else {
            $("#mode").removeClass('complete');
        }
        $(".chart").show();
    } else {
        d3.select("#mode").text(mode);
        $(".chart").hide();
    }
    d3.select("#" + mode + "-chemistry").text(d.chemistry);
    d3.select("#" + mode + "-amps").text(d.fast_amps.toFixed(2));
    d3.select("#" + mode + "-mah").text(d.mah_in);
    d3.select("#" + mode + "-mahout").text(d.mah_out);
    d3.select("#" + mode + "-volts").text(farray(d.voltage.splice(0, d.detected_cell_count)));
    d3.select("#" + mode + "-ir").text(farray(d.ir.splice(0, d.detected_cell_count)));
    d3.select("#" + mode + "-time").text(d.charge_time);

    $("#" + mode).show();

    batterygauge.redraw(d.avg_cell);
    cellchart();
}

function updateStatusNow() {
    d3.json("/statuses?after=" + lastStatus, function(d) {
        if (d) {
            $("body").removeClass('error');
            updateStatus(d);
        } else {
            $("body").addClass('error');
            console.log("Error updating");
        }
    });
}

function makeGauge() {
    var config = {
        size: 240,
        label: "",
        min: 0, max: 100,
        minorTicks: 5
    }

    var range = config.max - config.min;
    config.yellowZones = [{ from: config.min + range*0.8, to: config.min + range*0.95 }];
    config.greenZones = [{ from: config.min + range*0.95, to: config.max }];

    var g = new Gauge("chargeGauge", config);
    g.render();

    return g;
}

function makeCellChart() {
    var margin = {top: 20, right: 55, bottom: 30, left: 40},
    width  = 600 - margin.left - margin.right,
    height = 200  - margin.top  - margin.bottom;

    var x = d3.scale.ordinal()
        .rangeRoundBands([0, width], .1);

    var y = d3.scale.linear()
        .rangeRound([height, 0]);

    var line = d3.svg.line()
        .interpolate("cardinal")
        .x(function (d) { return x(d.label) + x.rangeBand() / 2; })
        .y(function (d) { return y(d.value); });

    var color = d3.scale.ordinal()
        .range(["#001c9c","#101b4d","#475003","#9c8305","#d3c47c"]);

    var svg = d3.select("#cellChart").append("svg")
        .attr("width",  width  + margin.left + margin.right)
        .attr("height", height + margin.top  + margin.bottom)
      .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    cellchart = function() {
        var values = [];
        var start = 0;
        var end = statuses.length;
        if (end > 300) {
            start = end - 300;
        }
        for (var i = start; i < end; i++) {
            var st = statuses[i].ST;
            for (var j = 0; j < st.voltage.length; j++) {
                var val = {
                    name: 'cell' + (1+j),
                    label: statuses[i].TS,
                    value: st.voltage[j],
                };
                var prev = values[j] ? values[j].values : [];
                prev.push(val);
                values[j] = {name: 'cell' + (j+1), values: prev};
            }
        }

        color.domain(values.map(function(d) { return d.name; }));

        seriesData = values;

        x.domain(values[0].values.map(function (d) { return d.label; }));
        y.domain([
          d3.min(seriesData, function (c) { 
            return d3.min(c.values, function (d) { return d.value; });
          }),
          d3.max(seriesData, function (c) { 
            return d3.max(c.values, function (d) { return d.value; });
          })
        ]);

        var series = svg.selectAll(".line").transition()
            .attr("d", function (d) { return line(d.values); })
            .duration(500);
         
        svg.selectAll(".line").data(seriesData).enter().append("path")
            .attr("class", "line")
            .style("stroke", function (d) { return color(d.name); })
            .style("stroke-width", "1px")
            .style("fill", "none")
            .attr("d", function (d) { return line(d.values); });
    };
}

function powerInit() {
    d3.select("#mode").text("init");
    $(".chart").hide();
    batterygauge = makeGauge();
    makeCellChart();
    updateStatusNow();

    setInterval(updateStatusNow, refreshInterval);
}
