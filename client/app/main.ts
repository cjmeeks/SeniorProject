import './scss/main.scss'
declare function require(name:string):any;
declare var moment: any;
declare var $: any;
var Elm = require("../elm/Main.elm");
var app = Elm.Main.fullscreen();
import '../assets/bootstrap/js/daterangepicker';

var datePicker = function(inputName, sDate, eDate){
  setTimeout( function() {
    $('input[name="' + inputName +'"]').daterangepicker({
          "timePicker": false,
          "showDropdowns": true,
          "autoApply": true,
          "linkedCalendars": false,
          "alwaysShowCalendars": true,
          "startDate": moment(sDate.toISOString()),
          "endDate": moment(eDate.toISOString()),
          ranges: {
             'Today': [moment(), moment()],
             'Yesterday': [moment().subtract(1, 'days'), moment().subtract(1, 'days')],
             'Last 7 Days': [moment().subtract(6, 'days'), moment()],
             'Last 30 Days': [moment().subtract(29, 'days'), moment()],
             'This Month': [moment().startOf('month'), moment().endOf('month')],
             'Last Month': [moment().subtract(1, 'month').startOf('month'), moment().subtract(1, 'month').endOf('month')],
          }
      }, function(start, end, label) {
          app.ports.handleDateChange.send("["+moment(start).format('YYYY-MM-DD HH:mm') + " TO " + moment(end).format('YYYY-MM-DD HH:mm')+"]");
      });
  }, 300);
};



app.ports.buildDatePicker.subscribe(function(input) {
  var name = input.name;
  var dates = parseDateRangeInput(input.dates);
  if(dates.startDate.toString() == "Invalid Date") {
    datePicker(name, moment().subtract(29, 'days'), moment());
  } else {
    // datePicker(name, moment(dates.startDate), moment(dates.endDate));
    datePicker(name, moment().subtract(29, 'days'), moment());
  }
  app.ports.handleDateChange.send("["+moment().subtract(29, 'days').format('YYYY-MM-DD HH:mm') + " TO " + moment().format('YYYY-MM-DD HH:mm')+"]");
});

function parseDateRangeInput(input) {
  var dates = input.toString().split(" TO ");
  if(dates.length < 2) {
    return {startDate : new Date("INVALID") , endDate : new Date("INVALID")};
  }
  var start = dates[0] == "*" ? new Date("1800-01-01") : new Date(dates[0]);
  var end = dates[1] == "*" ?  new Date("2500-01-01") : new Date(dates[1]);
  if(start.toString() == "Invalid Date" || end.toString() == "Invalid Date") {
    start = new Date("INVALID");
    end = new Date("INVALID");
  }
  return {startDate : start , endDate : end}
};

app.ports.getCurrentDate.subscribe(function(){
  app.ports.handleDateChange.send(moment().format('YYYY-MM-DD HH:mm'));
});