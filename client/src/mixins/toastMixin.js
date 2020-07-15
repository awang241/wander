export default {
    methods: {
        successToast(message) {
            this.$buefy.toast.open({
                duration: 5500,
                message: message,
                type: 'is-success',
                position: 'is-top'
            })
        },
        warningToast(message) {
            this.$buefy.toast.open({
                duration: 5500,
                message: message,
                type: 'is-danger',
                position: 'is-top'
            })
        },
    }
};