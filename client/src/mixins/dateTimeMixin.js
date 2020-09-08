export default {
    methods: {
        combineDateAndTime(date, time) {
            if (date === null|| time== null) {
                return null
            }
            let dateParts = date.split('-');
            let timeParts = time.split(':');

            if (dateParts && timeParts) {
                dateParts[1] -= 1;
                return new Date(Date.UTC.apply(undefined, dateParts.concat(timeParts))).toISOString();
            }
            return null;
        },
        /**
         * Converts a date-time string from the JSON format (yyyy-mm-ddThh:mm:ss(+|-)hhmm) to presentation format
         * (hh:hh dd/mm/yyyy). 24 hour time is used, and at the current stage time zone is ignored.
         *
         * For example, the string 2020-09-05T08:00:00+1300 is converted to 08:00 05/09/2020.
         * @param date - the date-time as a JSON format string.
         * @returns {string} the date-time string in presentation format.
         */
        dateFormat(date) {
            let year = date.slice(0, 4);
            let month = date.slice(5, 7);
            let day = date.slice(8, 10);
            let hour = date.slice(11, 13);
            let min = date.slice(14, 16);
            return hour + ":" + min + " " + day + "/" + month + "/" + year;
        }
    }
};