<template>
    <section>
        <div class="container">
                <h1>Create Account</h1>
                <form @submit.prevent="createUser" id="formRegister">

                    <b-field grouped>
                        <b-field label="First Name" expanded>
                            <b-input v-model="firstName" placeholder="First Name" required></b-input>
                        </b-field>
                        <b-field label="Middle Name" expanded>
                            <b-input v-model="middleName" placeholder="Middle Name"></b-input>
                         </b-field>
                        <b-field label="Last Name" expanded>
                            <b-input v-model="lastName" placeholder="Last Name" required></b-input>
                        </b-field>
                    </b-field>
                    <b-field grouped>
                        <b-field label="Email" expanded>
                                 <b-input type="email"
                                     v-model="email"
                                     placeholder="Email"
                                     name="email"
                                     id="email"
                                     maxlength="30"
                                     icon="email" required>
                                 </b-input>
                        </b-field>

                        <b-field label="Date of Birth" expanded>
                                 <b-datepicker
                                         placeholder="Select Date of Birth"
                                         icon="calendar-today"
                                         :max-date="maxDate" ref="dateOfBirth"
                                         v-model="dateOfBirth"
                                         type="date" required>
                                 </b-datepicker>
                        </b-field>
                    </b-field>
                    <b-field grouped>
                        <b-field label="Gender" expanded>
                                <b-select
                                        placeholder="Choose a gender"
                                        v-model="gender" required>
                                    <option value="female">Female</option>
                                    <option value="male">Male</option>
                                    <option value="nonBinary">Non Binary</option>
                                </b-select>
                        </b-field>

                        <b-field label="Fitness Level" expanded>
                            <b-select placeholder="Fitness Level" v-model="fitness">
                                <option value="0">Beginner: I am not active at all </option>
                                <option value="1">Novice: I do a low level excercise (walking)</option>
                                <option value="2">Intermediate: I work out 1-2 times per week </option>
                                <option value="3">Advanced: I work out 3-4 times per week</option>
                                <option value="4">Pro: I work out 5+ times per week</option>
                            </b-select>
                        </b-field>
                    </b-field>
                    <b-field grouped>
                        <b-field label="Password">
                            <b-input v-model="password" type="password" placeholder="Password" expanded required></b-input>
                        </b-field>

                        <b-field label="Confirm Password">
                            <b-input v-model="password" type="confpassword" placeholder="Confirm Password" expanded required></b-input>
                        </b-field>
                    </b-field>

<!--                    <b-field label="Nickname" label-position="on-border">-->
<!--                        <b-input v-model="nickName" type="text" placeholder="Nickname" maxlength="25"></b-input>-->
<!--                    </b-field>-->

<!--                    <b-field label="Bio" label-position="on-border">-->
<!--                        <b-input v-model="bio" type="textarea" placeholder="Nickname" maxlength="200"></b-input>-->
<!--                    </b-field>-->

<!--                    <button class="btn btn-light" type="submit">Submit</button>-->
<!--                    <p>* Indicates a required field</p>-->

                    <b-field><!-- Label left empty for spacing -->
                        <b-button  @click="createUser" type="submit">Submit</b-button>
                    </b-field>
                </form>

        </div>
    </section>



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
                .catch(error => window.alert(error.response.data))
            }
        }
    }
</script>


<style scoped>
    .card {
        min-height: 1000px;
    }

    h1 {
        padding: 1.5rem 0;
        font-weight: bold;
        text-align: center;
    }

    form {
        background-color: #FFFFFF;
        align-items: center;
        padding: 0 40px;
        text-align: left;
    }
</style>
