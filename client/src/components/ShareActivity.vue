<template>
    <div class="container">
        <h1 class="title">Share Activity</h1>
        <ValidationObserver v-slot="{ handleSubmit }">
            <form @submit.prevent="handleSubmit(shareActivity)">
                <ValidationProvider rules="required" name="activityPrivacy" v-slot="{ errors, valid }" slim>
                    <b-field label="Activity Privacy"
                             :type="{ 'is-danger': errors[0], 'is-success': valid }"
                             :message="errors"
                             expanded>
                        <template slot="label">Privacy<span>*</span></template>
                        <b-select v-model="privacy" placeholder="Choose privacy setting" expanded>
                            <option value="private">Private</option>
                            <option value="friends">Restricted</option>
                            <option value="public">Public</option>
                        </b-select>
                    </b-field>
                </ValidationProvider>

                <div v-if="privacy == 'friends'">
                    <ValidationProvider rules="email" name="Email" v-slot="{ errors, valid }" slim>
                        <b-field label="Add friend's emails"
                                 :type="{'is-danger': errors[0], 'is-success': valid}"
                                 :message="errors"
                                 expanded>
                            <b-input type="email" v-model="newEmail" placeholder="Enter a friend's email" maxlength="30"
                                     expanded></b-input>
                        </b-field>
                    </ValidationProvider>
                    <b-button class="addButton" type="is-primary" @click="addEmail()">Add</b-button>
                    <br>
                    <br>

                    <div v-for="email in emails" v-bind:key="email">
                        <div id="profile-container">
                            <b-button type="is-danger" size="is-small" @click="deleteEmail(email)">X</b-button>

                            <p>{{email}}</p>

                            <span>
                                <b-select v-model="role">
                                    <option value="follower">Follower</option>
                                    <option value="participant">Participant</option>
                                    <option value="organiser">Organiser</option>
                                </b-select>
                            </span>

                        </div>
                        <br>
                    </div>
                    <br>
                </div>
                <b-button style="float: right" @click="shareActivity"
                          type="is-primary">
                    Save
                </b-button>
                <b-button style="float: left" @click="goBack"
                          type="is-danger">
                    Cancel
                </b-button>
                <br>
            </form>
        </ValidationObserver>
    </div>
</template>


<script>
    import store from "../store";
    import router from "../router";
    import toastMixin from "../mixins/toastMixin";
    import {ValidationObserver, ValidationProvider} from "vee-validate";
    import Api from "../Api";

    export default {
        name: "ShareActivity",
        mixins: [toastMixin],
        components: {
            ValidationProvider,
            ValidationObserver
        },
        data() {
            return {
                privacy: null,
                emails: [],
                activityId: this.$route.params.id,
                newEmail: "",
                role: ""
            }
        },
        mounted() {
            this.checkAuthenticationStatus()
        },
        methods: {
            addEmail() {
                if (this.emails.includes(this.newEmail)) {
                    this.warningToast("Email address has already been added")
                } else if (this.newEmail === "" || this.newEmail.trim().length === 0 || !this.newEmail.includes('@', 0)) {
                    this.warningToast("Please enter a valid email address")
                } else {
                    console.log(this.emails);
                    this.emails.push(this.newEmail);
                    this.newEmail = "";
                }
            },
            shareActivity() {
                Api.editActivityPrivacy(store.getters.getUserId, this.$route.params.id, this.privacy, localStorage.getItem('authToken'))
                    .then((response) => {
                        console.log(response);
                        this.successToast("Activity privacy updated")
                        router.go(-1)
                    })
                    .catch(error => console.log(error));
            },

            deleteEmail(emailToDelete) {
                this.emails = this.emails.filter(email => email != emailToDelete)
            },

            goBack() {
                router.go(-1)
            },

            checkAuthenticationStatus() {
                if (!store.getters.getAuthenticationStatus) {
                    router.push({path: '/'})
                }
            },
        }
    }
</script>


<style scoped>
    .container {
        width: 800px;
    }

    @media only screen and (max-width: 600px) {
        .container {
            width: 100%;
        }
    }

    span {
        color: red;
    }

    #profile-container{
        display: flex;
        width: 100%;
        justify-content: space-between;
    }


</style>


