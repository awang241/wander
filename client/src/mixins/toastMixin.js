export default {
    methods: {
        successToast(message) {
            this.$buefy.toast.open({
                duration: 2000,
                message: message,
                type: 'is-success',
                position: 'is-top',
                queue: false
            })
        },
        warningToast(message) {
            this.$buefy.toast.open({
                duration: 2000,
                message: message,
                type: 'is-danger',
                position: 'is-top',
                queue: false
            })
        },
    }
};