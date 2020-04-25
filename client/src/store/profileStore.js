//Stores global state of the users profile
//Done this way to avoid complicated passing of data as props into seperate components
const profileStore = {
    data: {
        firstName: "",
        lastName: "",
        middleName: "",
        nickname: "",
        primaryEmail: "",
        optionalEmails:[],
        bio:"",
        dateOfBirth: "",
        gender: "",
        passportCountries: [],
        fitnessLevel: 1,
        activityTypes: [],
        allActivityTypes: []

    },
    methods: {
        setPassportCountries(passport_countries) {
            profileStore.data.passportCountries = passport_countries;
        },
        setPrimaryEmail(primary_email) {
            profileStore.data.primaryEmail = primary_email;
        },
        setActivityTypes(activityTypes) {
            profileStore.data.activityTypes = activityTypes;
        },
        setOptionalEmails(additional_email) {
            profileStore.data.optionalEmails = additional_email;
        },
        updatePersonal(updatedProfile) {
            profileStore.data.firstName = updatedProfile.firstname;
            profileStore.data.lastName = updatedProfile.lastname;
            profileStore.data.middleName = updatedProfile.middlename;
            profileStore.data.nickname = updatedProfile.nickname;
            profileStore.data.bio = updatedProfile.bio;
            profileStore.data.dateOfBirth = updatedProfile.date_of_birth;
            profileStore.data.gender = updatedProfile.gender;
            profileStore.data.fitnessLevel = updatedProfile.fitness;
        },
        setProfile(profile){
            profileStore.methods.updatePersonal(profile)
            profileStore.data.primaryEmail = profile.primary_email;
            profileStore.data.optionalEmails = profile.additional_email;
            profileStore.data.passportCountries = profile.passports;
            profileStore.data.activityTypes = profile.activities;
        },
        setAllActivityTypes(allActivityTypes) {
            profileStore.data.allActivityTypes = allActivityTypes.allActivityTypes;
        }
    }
};

export default profileStore