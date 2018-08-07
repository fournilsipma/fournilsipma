"use strict";

// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/includes
// https://tc39.github.io/ecma262/#sec-array.prototype.includes
if (!Array.prototype.includes) {
  Object.defineProperty(Array.prototype, 'includes', {
    value: function(searchElement, fromIndex) {

      if (this == null) {
        throw new TypeError('"this" is null or not defined');
      }

      // 1. Let O be ? ToObject(this value).
      var o = Object(this);

      // 2. Let len be ? ToLength(? Get(O, "length")).
      var len = o.length >>> 0;

      // 3. If len is 0, return false.
      if (len === 0) {
        return false;
      }

      // 4. Let n be ? ToInteger(fromIndex).
      //    (If fromIndex is undefined, this step produces the value 0.)
      var n = fromIndex | 0;

      // 5. If n ≥ 0, then
      //  a. Let k be n.
      // 6. Else n < 0,
      //  a. Let k be len + n.
      //  b. If k < 0, let k be 0.
      var k = Math.max(n >= 0 ? n : len - Math.abs(n), 0);

      function sameValueZero(x, y) {
        return x === y || (typeof x === 'number' && typeof y === 'number' && isNaN(x) && isNaN(y));
      }

      // 7. Repeat, while k < len
      while (k < len) {
        // a. Let elementK be the result of ? Get(O, ! ToString(k)).
        // b. If SameValueZero(searchElement, elementK) is true, return true.
        if (sameValueZero(o[k], searchElement)) {
          return true;
        }
        // c. Increase k by 1.
        k++;
      }

      // 8. Return false
      return false;
    }
  });
}

exports.pikadayNew = function (elementid) {
  return function(holidays) {
    var holidayarray = holidays.map(function(d) { return moment(d).valueOf(); });
    return function () {
      var date = moment().add(2, 'd').toDate();
      var picker = new Pikaday({
        field: document.getElementById(elementid),
        format: 'YYYY-MM-DD',
        defaultDate: date,
        minDate: date,
        disableDayFn: function (date) {
          var day = date.getDay();
          var mdate = moment(date).valueOf();
          return day === 0 || day === 1 || holidayarray.includes(mdate);
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
