//Stores global state of the users profile
//Done this way to avoid complicated passing of data as props into seperate components
const profileStore = {
    data: {
        firstName: "frank",
        lastName: ":)",
        middleName: "",
        nickname: ":)",
        primaryEmail: "",
        optionalEmails:[],
        bio:"",
        dateOfBirth: "",
        gender: "",
        passportCountries: [],
        fitnessLevel: 1

    },
    methods: {
        setPassportCountries(countries) {
            profileStore.data.passportCountries = countries;
        },
        setPrimaryEmail(primaryEmail) {
            profileStore.data.primaryEmail = primaryEmail;
        },
        setOptionalEmails(optionalEmails) {
            profileStore.data.optionalEmails = optionalEmails;
        },
        updatePersonal(updatedProfile) {
            profileStore.data.firstName = updatedProfile.firstName;
            profileStore.data.lastName = updatedProfile.lastName;
            profileStore.data.middleName = updatedProfile.middleName;
            profileStore.data.nickname = updatedProfile.nickname;
            profileStore.data.bio = updatedProfile.bio;
            profileStore.data.dateOfBirth = updatedProfile.dateOfBirth;
            profileStore.data.gender = updatedProfile.gender;
            profileStore.data.fitnessLevel = updatedProfile.fitnessLevel;
        },
        setProfile(profile){
            profileStore.methods.updatePersonal(profile)
            profileStore.data.primaryEmail = profile.primaryEmail;
            profileStore.data.optionalEmails = profile.optionalEmails;
            profileStore.data.passportCountries = profile.passport_countries;
        }
    }
};

export default profileStore