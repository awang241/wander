const authenticationStore = {
    data: {
        authenticated: false
    },
    methods: {
        setAuthenticated(isAuthenticated) {
            authenticationStore.data.authenticated = isAuthenticated;
        }
    }
};

export default authenticationStore