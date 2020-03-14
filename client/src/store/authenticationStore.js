//Stores global state of the application
//Currently only storing whether or not the user is authenticated
const authenticationStore = {
    data: {
        authenticated: false,
        userId: 0,
        sessionId: 0

    },
    methods: {
        setAuthenticated(isAuthenticated) {
            authenticationStore.data.authenticated = isAuthenticated;
        },
        isAuthenticated(){
            return authenticationStore.data.authenticated;
        },
        setUserId(userId) {
            authenticationStore.data.userId = userId;
        },
        getUserId(){
            return authenticationStore.data.userId;
        },
        setSessionId(sessionId) {
            authenticationStore.data.sessionId = sessionId;
        },
        getSessionId(){
            return authenticationStore.data.sessionId;
        }

    }
};

export default authenticationStore