<template>
    <div class="card">
        <div class="form-container sign-up-container">
            <h1>Create Account</h1>
            <form @submit.prevent="createUser" id="formRegister">
                <div class="required">
                    <input v-model="firstName" type="text" placeholder="First Name" name="First Name" id="firstName"
                           maxlength="25" required>
                </div>
                <div>
                    <input v-model="middleName" type="text" placeholder="Middle Name" name="Last Name" id="middleName"
                           maxlength="25">
                </div>
                <div class="required">
                    <input v-model="lastName" type="text" placeholder="Last Name" name="Last Name" id="lastName"
                           maxlength="25" required>
                </div>
                <div class="required">
                    <input v-model="email" type="email" placeholder="Email" name="email" id="email" maxlength="40"
                           required>
                </div>
                <div class="required">
                    <b-field>
                             <b-datepicker
                                     placeholder="Select Date of Birth"
                                     icon="calendar-today"
                                     :max-date="maxDate" ref="dateOfBirth"
                                     v-model="dateOfBirth"
                                     type="date"
                                     name="dateOfBirth"
                                     id="dateOfBirth" expanded required>
                             </b-datepicker>
                    </b-field>
                </div>

                <div class="required">
                        <b-field>
                            <b-select size="is-default"
                                      placeholder="Choose a gender"
                                      id="gender"
                                      v-model="gender" expanded required>
                                <option value="female">Female</option>
                                <option value="male">Male</option>
                                <option value="nonBinary">Non Binary</option>
                            </b-select>
                        </b-field>
                </div>

                <div>
                    <b-field>
                        <b-select size="is-default" placeholder="Fitness Level" id="fitness" name="fitness" v-model="fitness" expanded>
                            <option value="0">Beginner: I am not active at all </option>
                            <option value="1">Novice: I do a low level excercise (walking)</option>
                            <option value="2">Intermediate: I work out 1-2 times per week </option>
                            <option value="3">Advanced: I work out 3-4 times per week</option>
                            <option value="4">Pro: I work out 5+ times per week</option>
                        </b-select>
                    </b-field>
                </div>

                <div class="required">
                    <input v-model="password" placeholder="Password" type="password" name="password" id="password"
                           required>
                </div>
                <div>
                    <input type="text" name="nickname" placeholder="Nickname" id="nickname" maxlength="25"
                           v-model="nickName">
                    <textarea v-model="bio" placeholder="Bio" id="bio" name="bio" rows="2" cols="30"></textarea>
                </div>
                <button class="btn btn-light" type="submit">Submit</button>
                <p>* Indicates a required field</p>
            </form>

        </div>
    </div>
</template>

<script>
    import api from '../Api';
    import router from '../router.js'

    export default {
        name: "Registration",
        data() {
            const today = new Date()
            return {
                firstName: "",
                lastName: "",
                middleName: "",
                nickName: "",
                email: "",
                password: "",
                bio: "",
                dateOfBirth: "",
                gender: "",
                fitness: "",
                allUsers: null,
                date: new Date(),
                maxDate: new Date(today.getFullYear(), today.getMonth(), today.getDate() + 5)
            }
        },
        mounted() {
            let today = new Date().toISOString().split('T')[0];
            this.$refs.dateOfBirth.setAttribute('max', today);
        },
        methods: {
            createUser() {
                api.createProfile({
                    lastname: this.lastName,
                    firstname: this.firstName,
                    middlename: this.middleName,
                    nickname: this.nickName,
                    email: this.email,
                    password: this.password,
                    bio: this.bio,
                    date_of_birth: this.dateOfBirth,
                    gender: this.gender,
                    fitness_level: this.fitness,
                    passport_countries: []
                })
                .then((response => {
                    console.log(response)
                    router.push('Login')
                }))
                .catch(error => console.log(error))
            }
        }
    }
</script>


<style scoped>
    .card {
        min-height: 700px;
    }

    h1 {
        padding: 1.5rem 0;
        font-weight: bold;
        margin: 0;
        text-align: center;
    }

    h2 {
        text-align: center;
    }

    form div {
        width: 100%;
    }

    form {
        background-color: #FFFFFF;
        display: flex;
        align-items: center;
        justify-content: center;
        flex-direction: column;
        padding: 0 50px;
        text-align: left;
    }


</style>
