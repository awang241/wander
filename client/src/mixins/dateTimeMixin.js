export default {
    methods: {
        combineDateAndTime(date, time) {
            if (date === null) {
                return null
            }
            let dateParts = date.split('-')
            let timeParts = time.split(':')

            if (dateParts && timeParts) {
                dateParts[1] -= 1;
                return new Date(Date.UTC.apply(undefined, dateParts.concat(timeParts))).toISOString();
            }
            return null;
        }
    }
};