<template>
    <div class="container">
        <h1 class="title is-5">Edit Your Location</h1>

        <form>
            <b-field label="Country" expanded >
                <b-input v-model="country" placeholder="Country" required></b-input>
            </b-field>
            <b-field label="City" expanded>
                <b-input v-model="city" placeholder="City" required></b-input>
            </b-field>
            <b-field label="State" expanded>
                <b-input v-model="state" placeholder="State"></b-input>
            </b-field>
        </form>

        <div class="row">
            <b-field style="float:left">
                <b-button type="is-info" @click="submitLocation()">Save</b-button>
            </b-field>
            <b-field style="float:right">
                <b-button type="is-danger" @click="clearLocation()">Clear Location</b-button>
            </b-field>
        </div>
        <br/>
    </div>
</template>

<script>

    export default {
        name: "EditLocation",
        props: ["profile"],
        data() {
            return {
                country: this.profile.country,
                city: this.profile.city,
                state: this.profile.state,
            }
        },
        methods: {
            showWarning(message) {
                this.$buefy.toast.open({
                    duration: 5000,
                    message: message,
                    type: 'is-danger',
                    position: 'is-top',
                    queue: false,
                });
            },
            showSuccess(message){
                this.$buefy.toast.open({
                    duration: 2000,
                    message: message,
                    type: 'is-success',
                    position: 'is-top'
                })
            },
            clearLocation(){
                this.$parent.clearLocation()
                this.showSuccess("Location removed")
            },
            submitLocation(){
                this.$parent.updateLocation(this.country, this.city, this.state)
                this.showSuccess("Updated location")
            }
        }

    }

</script>

<style scoped>
    .container {
        background-color: #F7F8F9;
        margin-top: 0px;
        padding: 0px;
    }

    .button-right {
        align: right;
    }




</style>