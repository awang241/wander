<template>
    <div class="container">
        <h1 class="title">User search</h1>
        <form @submit.prevent="createUser">

            <b-field label="Name" expanded>
                <b-input type="text"
                         v-model="email"
                         placeholder="Name">
                </b-input>
            </b-field>

            <b-field label="Email" expanded>
                <b-input type="email"
                         v-model="email"
                         placeholder="Email"
                         required>
                </b-input>
            </b-field>

            <b-field label="Activity types">


                <b-taginput
                        v-model="tags"
                        :data="filteredTags"
                        autocomplete
                        field="user.first_name"
                        placeholder="Add a type"
                        @typing="getFilteredTags">
                    <template slot-scope="props">
                        <strong>{{props.option.id}}</strong>: {{props.option.user.first_name}}
                    </template>
                    <template slot="empty">
                        There are no items
                    </template>
                </b-taginput>
            </b-field>

            <div>
                <b-radio v-model="activity"
                         name="name"
                         native-value="Continuous">
                    Matching all types
                </b-radio>
                <b-radio v-model="activity"
                         name="name"
                         native-value="Duration">
                    Matching any type
                </b-radio>
            </div>

            <br>

            <div class="column">
                <div class="is-pulled-left">
                    <b-button type="is-danger" @click="goBack">Reset fields</b-button>
                </div>
                <div class="is-pulled-right">
                    <b-field>
                        <b-button native-type="submit" class="is-primary">Submit</b-button>
                    </b-field>

                </div>
            </div>

        </form>
    </div>
</template>

<script>
    export default {
        name: "ProfileSearch"
    }
</script>

<style scoped>

</style>