"use strict";

window['moment-range'].extendMoment(moment);

exports.pikadayNew = function (elementid) {
  return function(holidays) {
    return function () {
      var holidayranges = holidays.map(function(dr) {
        return moment.range(dr);
      });
      var date = moment().add(2, 'd').toDate();
      var picker = new Pikaday({
        field: document.getElementById(elementid),
        format: 'YYYY-MM-DD',
        defaultDate: date,
        minDate: date,
        disableDayFn: function (date) {
          var day = date.getDay();
          var holidayarray = holidayranges.filter(function(dr) {
            return dr.contains(moment(date))
          });
          return day === 0 || day === 1 || holidayarray.length > 0;
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
}

exports.stripeStripe = function (apikey) {
    return function () {
        return Stripe(apikey);
    };
}

exports.stripeElements = function (stripe) {
    return function () {
        return stripe.elements();
    };
}

exports.stripeCard = function (elements) {
    return function () {
        return elements.create('card', { 'hidePostalCode': true });
    };
}

exports.stripeCardMount = function (card) {
    return function (sel) {
        return function () {
            card.mount(sel);

            // TODO: rewrite in ps
            card.addEventListener('change', function(event) {
                var displayError = document.getElementById('fournil-card-errors');
                if (event.error) {
                    displayError.textContent = event.error.message;
                    displayError.style.display = 'block';
                } else {
                    displayError.textContent = '';
                    displayError.style.display = 'none';
                }
            });
        };
    };
}

exports.stripeCreateTokenRaw = function (stripe) {
    return function (card) {
        return function (left) {
            return function (right) {
                return function () {
                    return stripe.createToken(card).then(function(result) {
                        if (result.error) {
                            // Inform the user if there was an error
                            var errorElement = document.getElementById('fournil-card-errors');
                            // TODO: use config
                            errorElement.textContent = result.error.message;
                            return left(result.error.message);
                        } else {
                            // Set the tokenid
                            var tk = document.getElementById('fournil-card-tokenid');
                            tk.setAttribute('value', result.token.id);
                            return right(result.token.id);
                        }
                    });
                };
            };
        };
    };
}
