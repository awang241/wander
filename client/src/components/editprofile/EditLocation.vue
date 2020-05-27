<template>
    <div class="container">
        <h1 class="title is-5">Edit Your Location</h1>

        <form>
            <b-field label="Country" expanded >
                <b-input v-model="location.country" placeholder="Country" required></b-input>
            </b-field>
            <b-field label="City" expanded>
                <b-input v-model="location.city" placeholder="City" required></b-input>
            </b-field>
            <b-field label="State" expanded>
                <b-input v-model="location.state" placeholder="State"></b-input>
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

    import toastMixin from "../../mixins/toastMixin";

    export default {
        name: "EditLocation",
        props: ["profile"],
        mixins: [toastMixin],
        data() {
            return {
                location : {
                    country: "",
                    city: "",
                    state: "",
                }
            }
        },
        methods: {
            clearLocation(){
                this.$parent.clearLocation()
                this.successToast("Location removed")
            },
            submitLocation(){
                this.$parent.updateLocation(this.location)
                this.successToast("Updated location")
            },
            setLocation(){
                if(this.profile.location != null){
                    this.location = this.profile.location
                }
            }
        },
        mounted() {
            this.setLocation()
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