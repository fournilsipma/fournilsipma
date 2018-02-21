"use strict";

exports.pikadayNew = function (elementid) {
  return function () {
    var date = moment().add(2, 'd').toDate();
    var picker = new Pikaday({
      field: document.getElementById(elementid),
      format: 'YYYY-MM-DD',
      defaultDate: date,
      minDate: date,
      disableDayFn: function (date) {
        var day = date.getDay();
        return day === 0 || day === 1;
      },
      i18n: {
        previousMonth : 'Mois précédent',
        nextMonth     : 'Mois suivant',
        months        : ['Janvier','Février','Mars','Avril','Mai','Juin','Juillet','Août','Septembre','Octobre','Novembre','Decembre'],
        weekdays      : ['Dimanche','Lundi','Mardi','Mercredi','Jeudi','Vendredi','Samedi'],
        weekdaysShort : ['Dim','Lun','Mar','Mer','Jeu','Ven','Sam'],
      },
      firstDay: 1
    });
    return picker;
  }
}
