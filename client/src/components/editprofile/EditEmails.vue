<template>

    <div class="container">
        <h1 class="Title">Edit Email Addresses</h1>

        <h2>
            Current Primary Email Address
        </h2>

        <label class="primaryEmailText primaryEmailBox">
            {{primaryEmail}}
        </label>

        <form>
            <b-field label="Enter in an email address and click the + sign to add it to your profile! (5 email limit)" class="addEmailsText">
            </b-field>
            <b-input type="email" class="addForm" v-model="newEmail" placeholder="n emails left" maxlength="30" ></b-input>
            <b-button class="addButton" type="is-info" @click="addEmail()">
                +
            </b-button>
        </form>

        <b-field label="Change your primary email">
            <b-select v-model="newPrimaryEmail" class="selectNewPEList">
                <option class="singleEmail" v-for="email in optionalEmails" :key="email">{{email}}</option>
            </b-select>

        </b-field>
        <b-button class="changeButton changeGroup" size="is-small" type="is-info" @click="changePrimaryEmail()">
            Change
        </b-button>

        <list v-bind:chosenItems="optionalEmails" v-on:deleteListItem="deleteEmail"></list>

        <b-field>
            <b-button native-type="submit" @click="submitEmails">Save</b-button>
        </b-field>
        </form>
        <b-button @click="saveEmails()"> Save </b-button>
    </div>
</template>

<script>

    import List from "../List";
    import authenticationStore from "../../store/authenticationStore";
    import Api from "../../Api";
    import authenticationStore from "../../store/authenticationStore";
    export default {
        name: "EditEmails",
        components: {List},
        methods: {
            addEmail() {
                if(this.optionalEmails.length > 3){
                    //Todo change this to inform the user
                  this.showWarning("Maximum emails reached")
                } else if(this.optionalEmails.includes(this.newEmail) || this.newEmail === this.primaryEmail){
                    this.showWarning("Email is already in use")
                } else {
                    this.optionalEmails.push(this.newEmail);
                }
            },
            changePrimaryEmail() {
                if(this.newPrimaryEmail === "") {
                    this.showWarning("No email selected")
                } else {
                    this.optionalEmails.push(this.primaryEmail);
                    this.optionalEmails = this.optionalEmails.filter(email => email != this.newPrimaryEmail)
                    this.primaryEmail = this.newPrimaryEmail;
                }
            },

            deleteEmail(emailToDelete){
                this.optionalEmails = this.optionalEmails.filter(email => email != emailToDelete)
            },
            saveEmails() {
                api.editEmail({
                        primaryEmail: this.primaryEmail,
                        optionalEmails: this.optionalEmails
                    },
                    authenticationStore.methods.getUserId(),
                    authenticationStore.methods.getSessionId())
                }
            },
            showWarning(message) {
                this.$buefy.snackbar.open({
                    duration: 5000,
                    message: message,
                    type: 'is-danger',
                    position: 'is-bottom-left',
                    queue: false,
                })
            },
            mounted() {
                api.getProfile(authenticationStore.methods.getUserId(), authenticationStore.methods.getSessionId())
                    .then((response) => {
                        this.primaryEmail = response.data.email;
                        console.log(this.primaryEmail)
                    })
            },
            data() {
                return {
                    primaryEmail: "replacewithUserRegisteredEmail@gmail.com",
                    newEmail: "",
                    newPrimaryEmail: "",
                    optionalEmails: [],
                }
            }
        }
            showSuccess(message){
                this.$buefy.toast.open({
                    duration:5500,
                    message: message,
                    type: 'success',
                    position: 'is-bottom'
                })
            },
            submitEmails(){

                Api.editEmail({
                    "primary_email": this.primaryEmail,
                    "additional_email": this.optionalEmails
                }, authenticationStore.methods.getUserId(), authenticationStore.methods.getSessionId())
                this.showSuccess("Emails submitted")
            }
        },
        data() {
            return {
            primaryEmail: "",
            newEmail: "",
            newPrimaryEmail: "",
            optionalEmails: [],
            }
        }
    }
</script>

<style scoped>
    .container {
        background-color: #F7F8F9;
    }

</style>