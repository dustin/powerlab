var refreshInterval = 5000;
var maxStatuses = 3600;

var statuses = [];
var lastStatus = '';

var batterygauge;
var cellchart;

var lastReading = new Date();

function sp(d) {
    return d.detected_cell_count + "S" + d.packs + "P";
}

function farray(a, f) {
    var rv = [];
    for (var i = 0; i < a.length; i++) {
        rv.push(f(a[i]));
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
        if (new Date() - lastReading > 30000) {
            d3.select("#mode").text("no status");
        }
        return;
    }

    lastReading = new Date();

    $(".modesec").hide();

    if (statuses.length > 0 && statuses[statuses.length-1].ST.charge_sec > dees[0].ST.charge_sec) {
        console.log("Resetting the statuses since charge time dropped from",
                    statuses[statuses.length-1].ST.charge_time, "to",
                    dees[0].ST.charge_time);
        statuses = [];
    }

    statuses = statuses.concat(dees);
    statuses.sort(function(a, b) { return a.TS < b.TS ? -1 : 1; });
    if (statuses.length > maxStatuses) {
        statuses = statuses.splice(statuses.length-maxStatuses);
    }

    var last = dees[dees.length-1];
    var d = last.ST;
    lastStatus = last.TS;

    var mode = d.mode.replace(/ /g, '-');

    if (mode == 'charging' || mode == 'discharging' || mode == 'pack-cool-down' || mode == 'monitoring') {
        d3.select("#mode").text(d.mode + " " + sp(d) + " " + d.avg_cell.toFixed(1) + "%");
        if (d.charge_complete) {
            $("#mode").addClass('complete');
        } else {
            $("#mode").removeClass('complete');
        }
        $(".chart").show();
    } else {
        d3.select("#mode").text(d.mode);
        $(".chart").hide();
    }
    d3.select("title").text("PowerServer - " + d3.select("#mode").text());
    d3.select("#" + mode + "-chemistry").text(d.chemistry);
    d3.select("#" + mode + "-amps").text(d3.format(".3s")(d.fast_amps) + "A");
    d3.select("#" + mode + "-mah").text(d3.format(".3s")(d.mah_in / 1000) + "Ah");
    d3.select("#" + mode + "-mahout").text(d3.format(".3s")(d.mah_out / 1000) + "Ah");
    d3.select("#" + mode + "-maxcell").text(d3.format(".3s")(d.max_cell) + "V");
    d3.select("#" + mode + "-volts").text(farray(d.voltage.splice(0, d.detected_cell_count),
                                                function(x) { return d3.format(".3s")(x) + "V";}));
    d3.select("#" + mode + "-ir").text(farray(d.ir.splice(0, d.detected_cell_count),
                                             function(x) { return d3.format(".3s")(x / 1000) + "Ω";}));
    d3.select("#" + mode + "-time").text(d.charge_time);

    d3.select("#cycle-num").text(d.cycle_num);
    if (d.cycle_num > 0) {
        $(".cycle-info").show();
    } else {
        $(".cycle-info").hide();
    }

    $("#" + mode).show();

    batterygauge.redraw(d.avg_cell);
    cellchart();
}

function updateStatusNow() {
    d3.json("/statuses?after=" + lastStatus, function(e, d) {
        if (d) {
            $("body").removeClass('error');
            updateStatus(d);
        } else {
            $("body").addClass('error');
            console.log("Error updating", e);
        }
    });
}

function makeGauge() {
    var config = {
        size: 240,
        label: "",
        min: 0, max: 100,
        minorTicks: 5,
        yellowZones: [{from: 80, to: 95}],
        greenZones: [{from: 95, to: 100}]
    }

    var g = new Gauge("chargeGauge", config);
    g.render();

    return g;
}

function makeCellChart() {
    var chart;

    nv.addGraph(function() {
        chart = nv.models.lineChart()
            .useInteractiveGuideline(true);

        chart.xAxis
            .axisLabel('Charge Time (s)')
            .tickFormat(d3.format(',r'));

        chart.yAxis
            .axisLabel('Voltage (v)')
            .tickFormat(d3.format('.02f'));

        nv.utils.windowResize(chart.update);

        return chart;
    });

    cellchart = function() {
        var values = [];
        for (var i = 0; i < statuses.length; i++) {
            var st = statuses[i].ST;
            if (st.mode == 'detecting pack') {
                continue;
            }

            for (var j = 0; j < st.voltage.length; j++) {
                if (j >= values.length) {
                    values.push({
                        key: 'Cell ' + (1+j),
                        values: [],
                    });
                }
                values[j].values.push({x: st.charge_sec, y: st.voltage[j]});
            }
        }
        seriesData = values;

        d3.select('#cellChart svg')
            .datum(seriesData)
            .transition().duration(500)
            .call(chart);
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
