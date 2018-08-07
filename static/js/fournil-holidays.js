window['moment-range'].extendMoment(moment);

moment.locale('fr');

function formatRange(dr) {
  var formatStart = (dr.start.month() == dr.end.month()) ? "Do" : "Do MMMM";
  return "<span class=\"font-weight-bold\">du " + dr.start.format(formatStart) + " au " + dr.end.format("Do MMMM") + "</span>"
};
