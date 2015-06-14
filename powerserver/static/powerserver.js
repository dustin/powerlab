var refreshInterval = 5000;

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

function updateStatus(d) {
    $(".modesec").hide();

    if (d.mode == 'charging' || d.mode == 'discharging' || 'pack cool down') {
        d3.select("#mode").text(d.mode + " " + sp(d) + " " + d.avg_cell.toFixed(1) + "%");
        if (d.charge_complete) {
            $("#mode").addClass('complete');
        } else {
            $("#mode").removeClass('complete');
        }
    } else {
        d3.select("#mode").text(d.mode);
    }
    d3.select("#" + d.mode + "-chemistry").text(d.chemistry);
    d3.select("#" + d.mode + "-amps").text(d.fast_amps.toFixed(2));
    d3.select("#" + d.mode + "-mah").text(d.mah_in);
    d3.select("#" + d.mode + "-volts").text(farray(d.voltage.splice(0, d.detected_cell_count)));
    d3.select("#" + d.mode + "-ir").text(farray(d.ir.splice(0, d.detected_cell_count)));
    d3.select("#" + d.mode + "-time").text(d.charge_time);

    $("#" + d.mode).show();
}

function updateStatusNow() {
    d3.json("/status", function(d) {
        if (d) {
            $("body").removeClass('error');
            updateStatus(d);
        } else {
            $("body").addClass('error');
            console.log("Error updating");
        }
    });
}

function powerInit() {
    d3.select("#mode").text("init");
    updateStatusNow();

    setInterval(updateStatusNow, refreshInterval);
}
