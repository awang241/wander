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
                date = new Date(Date.UTC.apply(undefined, dateParts.concat(timeParts)));
                const adjustedDate = new Date(date.getTime() + date.getTimezoneOffset() * 60000);
                return adjustedDate.toISOString()
            }
            return null;
        },
        /**
         * Converts a date-time string from the JSON format (yyyy-mm-ddThh:mm:ss(+|-)hhmm) to presentation format
         * (hh:hh dd/mm/yyyy). 24 hour time is used, and the date is adjusted to UTC.
         *
         * For example, the string 2020-09-05T19:00:00+1300 is converted to 06:00 05/09/2020.
         * @param dateTimeString - the date-time as a JSON format string.
         * @returns {string} the date-time string in presentation format.
         */
        dateTimeFormat(dateTimeString) {
            let date = new Date(dateTimeString);
            let timeDateArray =  date.toLocaleDateString('en-NZ', {
                hour12: false,
                year: "numeric",
                month: "numeric",
                day: "numeric",
                hour: "numeric",
                minute: "numeric"
            }).split(',').reverse();
            if (date.getDate() < 10){
                timeDateArray[1] = '0' + timeDateArray[1]
            }
            return timeDateArray.join(' ').trim();
        },
        getTimeZone(dateString) {
            return dateString.split("+")[1]
        }
    }
};