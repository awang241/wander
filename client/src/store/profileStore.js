//Stores global state of the users profile
//Done this way to avoid complicated passing of data as props into seperate components
const profileStore = {
    data: {
        firstName: "f",
        lastName: "l",
        middleName: "m",
        nickname: "n",
        primaryEmail: "p",
        optionalEmails:"o",
        bio:"b",
        dateOfBirth: "d",
        gender: "g",
        passportCountries: ["China", "Russia"],
        fitnessLevel: "f"

    },
    methods: {
        setPassportCountries(countries) {
            profileStore.data.passportCountries = countries;
        },


    }
};

export default profileStore