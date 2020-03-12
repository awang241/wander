//Stores global state of the application
//Currently only storing whether or not the user is authenticated
const authenticationStore = {
    data: {
        authenticated: false
    },
    methods: {
        setAuthenticated(isAuthenticated) {
            authenticationStore.data.authenticated = isAuthenticated;
        },
        isAuthenticated(){
            return authenticationStore.data.authenticated;
        }
    }
};

export default authenticationStore